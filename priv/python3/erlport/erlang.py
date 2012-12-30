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

import sys
from sys import exc_info
from threading import Lock
from traceback import extract_tb

from erlport import Atom

class Error(Exception):
    """ErlPort Error."""

class InvalidMessage(Error):
    """Invalid message."""

class UnknownMessage(Error):
    """Unknown message."""

class CallError(Error):
    """Call error."""

    def __init__(self, value):
        if type(value) != tuple or len(value) != 4:
            value = None, None, value, []
        self.language, self.type, self.value, self.stacktrace = value
        Error.__init__(self, value)


class MessageHandler(object):

    def __init__(self, port):
        self.port = port
        self.set_encoder(None)
        self.set_decoder(None)
        call_lock = Lock()
        self._call_lock_acquire = call_lock.acquire
        self._call_lock_release = call_lock.release

    def set_encoder(self, encoder):
        if encoder:
            self.encoder = self.object_iterator(encoder)
        else:
            self.encoder = lambda o: o

    def set_decoder(self, decoder):
        if decoder:
            self.decoder = self.object_iterator(decoder)
        else:
            self.decoder = lambda o: o

    def object_iterator(self, handler,
            isinstance=isinstance, list=list, tuple=tuple, map=map):
        def iterator(obj):
            obj = handler(obj)
            if isinstance(obj, (list, tuple)):
                return obj.__class__(map(iterator, obj))
            return obj
        return iterator

    def start(self):
        call = self.call_with_error_handler
        try:
            self.loop(self.port.read, self.port.write, call)
        except EOFError:
            pass

    def loop(self, read, write, call):
        while True:
            message = read()
            try:
                mtype, module, function, args = message
            except ValueError:
                raise InvalidMessage(message)

            if mtype != b"C":
                raise UnknownMessage(message)

            write(call(module, function, args))

    def cast(self, pid, message):
        # It safe to call it from multiple threads because port.write will be
        # locked
        self.port.write((Atom(b'M'), pid, message))

    def call(self, module, function, args):
        if not isinstance(module, Atom):
            raise ValueError(module)
        if not isinstance(function, Atom):
            raise ValueError(function)
        if not isinstance(args, list):
            raise ValueError(args)

        self._call_lock_acquire()
        try:
            self.port.write((Atom(b"C"), module, function,
                # TODO: Optimize list(map())
                list(map(self.encoder, args))))
            response = self.port.read()
        finally:
            self._call_lock_release()
        try:
            mtype, value = response
        except ValueError:
            raise InvalidMessage(response)

        if mtype != b"r":
            if mtype == b"e":
                raise CallError(value)
            raise UnknownMessage(response)
        return self.decoder(value)

    def call_with_error_handler(self, module, function, args):
        try:
            mod = module.decode()
            objects = function.decode().split(".")
            f = sys.modules.get(mod)
            if not f:
                f = __import__(mod, {}, {}, [objects[0]])
            for o in objects:
                f = getattr(f, o)
            result = Atom(b"r"), self.encoder(f(*map(self.decoder, args)))
        except:
            t, val, tb = exc_info()
            exc = Atom(bytes("%s.%s" % (t.__module__, t.__name__), "utf-8"))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            result = Atom(b"e"), (Atom(b"python"), exc, str(val), exc_tb)
        return result

class RedirectedStdin(object):

    def __getattr__(self, name):
        def closed(*args, **kwargs):
            raise RuntimeError("STDIN is closed for ErlPort connected process")
        return closed

class RedirectedStdout(object):

    def __init__(self, port):
        self.__port = port

    def write(self, data):
        if not isinstance(data, str):
            raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.__port.write((Atom(b"P"), data))

    def writelines(self, lst):
        for data in lst:
            if not isinstance(data, str):
                raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.write("".join(lst))

    def __getattr__(self, name):
        def unsupported(*args, **kwargs):
            raise RuntimeError("unsupported STDOUT operation for ErlPort"
                " connected process")
        return unsupported


def setup_stdin_stdout(port):
    global RedirectedStdin, RedirectedStdout
    sys.stdin = RedirectedStdin()
    sys.stdout = RedirectedStdout(port)
    del RedirectedStdin, RedirectedStdout

def setup_api_functions(handler):
    global call, cast, set_encoder, set_decoder
    call = handler.call
    cast = handler.cast
    set_encoder = handler.set_encoder
    set_decoder = handler.set_decoder

def setup(port):
    global MessageHandler, setup
    handler = MessageHandler(port)
    setup_api_functions(handler)
    setup_stdin_stdout(port)
    del MessageHandler, setup
    handler.start()
