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
from traceback import extract_tb

from erlport import Atom

class Error(Exception):
    """ErlPort Error."""

class InvalidMessage(Error):
    """Invalid message."""

class UnknownMessage(Error):
    """Unknown message."""

class InvalidArgument(Error):
    """Invalid argument."""

class InvalidMode(Error):
    """Invalid mode."""

class ErlangError(Error):
    """Erlang error()."""

    def __init__(self, value, stacktrace):
        self.value = value
        self.stacktrace = stacktrace
        Error.__init__(self, (value, stacktrace))

class ThrowErlangError(ErlangError):
    """Erlang throw()."""

class ExitErlangError(ErlangError):
    """Erlang exit()."""


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
            mtype, module, function, args = message
        except ValueError:
            raise InvalidMessage(message)
        else:
            if mtype == "C":
                write(self.call_with_error_handler(module, function, args))
            elif mtype == "S":
                write(Atom("s"))
                self.client = True
                try:
                    write(self.call_with_error_handler(module, function, args))
                finally:
                    self.client = False
            else:
                raise UnknownMessage(message)

    def call(self, module, function, args):
        if not self.client:
            raise InvalidMode("call() is unsupported in server mode")

        if not isinstance(module, Atom):
            raise InvalidArgument(module)
        if not isinstance(function, Atom):
            raise InvalidArgument(function)
        if not isinstance(args, list):
            raise InvalidArgument(args)

        # TODO: Conver EOFError to some other exception?
        self.port.write((Atom('C'), module, function, args))
        response = self.port.read()
        try:
            mtype, value = response
        except ValueError:
            raise InvalidMessage(response)

        if mtype != "r":
            if mtype == "e":
                # TODO: Raise error based on error value
                raise Exception("error")
            raise UnknownMessage(response)
        return value

    def call_with_error_handler(self, module, function, args):
        # TODO: Need to check this code
        try:
            f = modules.get(module)
            if f is None:
                f = __import__(module, {}, {}, [function])
            f = getattr(f, function)
            result = Atom("r"), f(*args)
        except:
            # TODO: Update exception format
            t, val, tb = exc_info()
            exc = Atom("%s.%s" % (t.__module__, t.__name__))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            result = Atom("e"), (exc, unicode(val), exc_tb)
        return result


def start(port):
    global MessageHandler, call
    handler = MessageHandler(port)
    call = handler.call
    del MessageHandler
    handler.start()
