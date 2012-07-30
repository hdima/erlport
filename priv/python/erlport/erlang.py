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
                self.client = True
                write(Atom("s"))
                try:
                    self._call(module, function, args)
                except:
                    t, val, tb = exc_info()
                    exc = Atom("%s.%s" % (t.__module__, t.__name__))
                    exc_tb = extract_tb(tb)
                    exc_tb.reverse()
                    result = Atom("e"), (exc, unicode(val), exc_tb)
                    write(result)
                else:
                    write(Atom("S"))
                finally:
                    self.client = False

    def call(self, module, function, args):
        # TODO: Check all arguments
        if not self.client:
            raise ErlangError("call() is unsupported in server mode")
        request = Atom('C'), module, function, args
        self.port.write(request)
        try:
            response = self.port.read()
        except EOFError:
            # TODO: Raise some other error?
            raise

        try:
            mtype, value = response
        except ValueError:
            # TODO: Raise some other error?
            raise

        if mtype != "r":
            if mtype == "e":
                # TODO: Raise error based on error value
                raise Exception("error")
            # TODO: Raise some other error?
            raise Exception("unknown message")
        return value

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
