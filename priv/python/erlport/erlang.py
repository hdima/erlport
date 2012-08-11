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
from traceback import extract_tb

from erlport import Atom

class Error(Exception):
    """ErlPort Error."""

class InvalidMessage(Error):
    """Invalid message."""

class UnknownMessage(Error):
    """Unknown message."""

class InvalidMode(Error):
    """Invalid mode."""

class ErlangError(Error):
    """Erlang error()."""

    def __init__(self, type, value, stacktrace):
        self.type = type
        self.value = value
        self.stacktrace = stacktrace
        Error.__init__(self, (type, value, stacktrace))


class MessageHandler(object):

    def __init__(self, port):
        self.port = port
        self.client = False
        self.set_encoder(None)
        self.set_decoder(None)

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
        switch_ack = Atom("s")
        while True:
            message = read()
            try:
                mtype, module, function, args = message
            except ValueError:
                raise InvalidMessage(message)

            if mtype == "C":
                write(call(module, function, args))
            elif mtype == "S":
                write(switch_ack)
                self.client = True
                write(call(module, function, args))
                self.client = False
            else:
                raise UnknownMessage(message)

    def call(self, module, function, args):
        if not self.client:
            raise InvalidMode("call() is unsupported in server mode")

        if not isinstance(module, Atom):
            raise ValueError(module)
        if not isinstance(function, Atom):
            raise ValueError(function)
        if not isinstance(args, list):
            raise ValueError(args)

        self.port.write((Atom('C'), module, function, map(self.encoder, args)))
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
        return self.decoder(value)

    def call_with_error_handler(self, module, function, args):
        # TODO: Need to check this code
        try:
            f = sys.modules.get(module)
            if not f:
                f = __import__(module, {}, {}, [function])
            f = getattr(f, function)
            result = Atom("r"), self.encoder(f(*map(self.decoder, args)))
        except:
            # TODO: Update exception format
            t, val, tb = exc_info()
            exc = Atom("%s.%s" % (t.__module__, t.__name__))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            result = Atom("e"), (exc, unicode(val), exc_tb)
        return result

class Function(object):

    __slots__ = ()

    def __new__(cls, name, module):
        cls.__call__ = lambda s, *args: call(module, name, list(args))
        return super(Function, cls).__new__(cls)

class Module(object):

    __slots__ = ()

    def __new__(cls, name):
        cls.__getattribute__ = lambda s, fname: Function(Atom(fname), name)
        return super(Module, cls).__new__(cls)

class Modules(object):

    __slots__ = ()

    def __getattribute__(self, module):
        return Module(Atom(module))

modules = Modules()
del Modules


def start(port):
    global MessageHandler, start, call, set_encoder, set_decoder
    handler = MessageHandler(port)
    call = handler.call
    set_encoder = handler.set_encoder
    set_decoder = handler.set_decoder
    del MessageHandler, start
    handler.start()
