# Copyright (c) 2009-2012, Dmitry Vasiliev <dima@hlabs.org>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  * Neither the name of the copyright holders nor the names of its
#    contributors may be used to endorse or promote products derived from this
#    software without specific prior written permission. 
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

from sys import modules, exc_info
from Queue import Queue
from threading import Thread
from traceback import extract_tb

from erlport import Atom


class Error(Exception):

    def __init__(self, value, stacktrace):
        self.value = value
        self.stacktrace = stacktrace
        Exception.__init__(self, (value, stacktrace))

class ErlangError(Error):
    """Erlang error."""

class ThrowErlangError(Error):
    """Erlang throw()."""

class ExitErlangError(Error):
    """Erlang exit()."""


class CallThread(Thread):

    def __init__(self, sender):
        self.sender = sender
        self.commands = Queue()
        Thread.__init__(self)
        self.setDaemon(True)

    def call(self, id, module, function, args):
        self.commands.put((id, module, function, args))

    def cast(self, module, function, args):
        self.commands.put((module, function, args))

    def run(self):
        while True:
            command = self.commands.get()
            if len(command) == 4:
                id, module, function, args = command
                result = self._call_with_errors(module, function, args)
                self.sender.send((Atom("R"), id, result))
            elif len(command) == 3:
                module, function, args = command
                self._call(module, function, args)

    def _call_with_errors(self, module, function, args):
        try:
            result = Atom("ok"), self._call(module, function, args)
        except:
            t, val, tb = exc_info()
            exc = Atom("%s.%s" % (t.__module__, t.__name__))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            result = Atom("error"), (exc, unicode(val), exc_tb)
        return result

    def _call(self, module, function, args):
        objects = function.split(".")
        f = modules.get(module)
        if f is None:
            f = __import__(module, {}, {}, [objects[0]])
        for o in objects:
            f = getattr(f, o)
        return f(*args)



class ReceiveThread(Thread):

    def __init__(self, port, sender, manager):
        self.port = port
        self.manager = manager
        self.calls = CallThread(sender)
        self.calls.start()
        Thread.__init__(self)
        self.setDaemon(True)

    def run(self):
        while True:
            try:
                message = self.port.read()
            except EOFError:
                break
            self.handle(message)

    def handle(self, message):
        if isinstance(message, tuple) and len(message) >= 3:
            mtype = message[0]
            if isinstance(mtype, Atom):
                if mtype == "S":
                    if len(message) == 5:
                        id, module, function, args = message[1:]
                        self.calls.call(id, module, function, args)
                elif mtype == "A":
                    if len(message) == 4:
                        module, function, args = message[1:]
                        self.calls.cast(module, function, args)
                elif mtype == "R":
                    if len(message) == 3:
                        id, result = message[1:]
                        queue = self.manager.requests[id]
                        if queue is not None:
                            queue.put(result)


class SendThread(Thread):

    def __init__(self, port):
        self.port = port
        self.requests = Queue()
        Thread.__init__(self)
        self.setDaemon(True)

    def run(self):
        while True:
            message = self.requests.get()
            try:
                self.port.write(message)
            except EOFError:
                break

    def send(self, message):
        self.requests.put(message)


class ThreadedCallProtocol(object):

    def __init__(self, port):
        self.id = 0
        self.requests = {}
        self.port = port
        self.sender = SendThread(self.port)
        self.sender.start()
        self.receiver = ReceiveThread(self.port, self.sender, self)
        self.receiver.start()

    def cast(self, module, function, args):
        request = Atom("A"), Atom(module), Atom(function), list(args)
        self.sender.send(request)

    def call(self, module, function, args):
        id = self.id
        # TODO: Optimize id generation
        # TODO: Cleanup requests storage
        # TODO: Lock?
        self.id += 1
        request = Atom("S"), id, Atom(module), Atom(function), list(args)
        queue = Queue()
        self.requests[id] = queue
        self.sender.send(request)
        result = queue.get()
        del self.requests[id]
        if result[0] == Atom("ok"):
            return result[1]
        error, value, stacktrace = result[1:4]
        if error == Atom("error"):
            raise ErlangError(value, stacktrace)
        elif error == Atom("exit"):
            raise ExitErlangError(value, stacktrace)
        elif error == Atom("throw"):
            raise ThrowErlangError(value, stacktrace)
        raise Error(value, stacktrace)

    def join(self):
        self.receiver.join()
        self.sender.join()

class MessageHandler(object):

    def __init__(self, port):
        self.port = port
        self.client = False

    def start(self):
        read = self.port.read
        write = self.port.write
        handle = self.handle
        while True:
            try:
                handle(read(), write)
            except EOFError:
                break

    def handle(self, message, write):
        try:
            # TODO: Is it faster than checking the type and size first?
            mtype, module, function, args = message
        except ValueError:
            # FIXME: Should we exit if we received a bad message?
            pass
        else:
            # TODO: Check mode
            if mtype == "C":
                write(self._call_with_error_handler(module, function, args))
            elif mtype == "S":
                write(Atom("s"))
                try:
                    self._call(module, function, args)
                except:
                    # TODO: Switch function result should be passed to
                    # Erlang and Python should switch to server mode
                    pass

    def call(self, module, function, args):
        raise ErlangError("call() is unsupported in server mode")

    def _call_with_error_handler(self, module, function, args):
        # TODO: Need to check this code
        try:
            result = self._call(module, function, args)
        except:
            t, val, tb = exc_info()
            exc = Atom("%s.%s" % (t.__module__, t.__name__))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            result = Atom("e"), (exc, unicode(val), exc_tb)
        return result

    def _call(self, module, function, args):
        # TODO: Need to check this code
        f = modules.get(module)
        if f is None:
            f = __import__(module, {}, {}, [function])
        f = getattr(f, function)
        return Atom("r"), f(*args)


def start(port):
    global MessageHandler, call
    handler = MessageHandler(port)
    call = handler.call
    del MessageHandler
    handler.start()
