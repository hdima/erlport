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

class InvalidArgument(Error):
    """Invalid argument."""

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
        self.encoder = None
        self.decoder = None

    def set_encoder(self, encoder):
        if encoder:
            encoder = self.object_iterator(encoder)
        self.encoder = encoder

    def set_decoder(self, decoder):
        if decoder:
            decoder = self.object_iterator(decoder)
        self.decoder = decoder

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
            raise InvalidArgument(module)
        if not isinstance(function, Atom):
            raise InvalidArgument(function)
        if not isinstance(args, list):
            raise InvalidArgument(args)

        encode = self.encoder
        if encode:
            req = Atom('C'), module, function, map(encode, args)
        else:
            req = Atom('C'), module, function, args
        self.port.write(req)
        response = self.port.read()
        try:
            mtype, value = response
        except ValueError:
            raise InvalidMessage(response)

        decode = self.decoder
        if mtype != "r":
            if mtype == "e":
                # TODO: Raise error based on error value
                # TODO: Decode exception terms
                raise Exception("error")
            raise UnknownMessage(response)
        if decode:
            return decode(value)
        return value

    def call_with_error_handler(self, module, function, args):
        # TODO: Need to check this code
        try:
            f = sys.modules.get(module)
            if not f:
                f = __import__(module, {}, {}, [function])
            f = getattr(f, function)
            decode = self.decoder
            if decode:
                r = f(*map(decode, args))
            else:
                r = f(*args)
            encode = self.encoder
            if encode:
                result = Atom("r"), encode(r)
            else:
                result = Atom("r"), r
        except:
            # TODO: Update exception format
            t, val, tb = exc_info()
            exc = Atom("%s.%s" % (t.__module__, t.__name__))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            e = exc, unicode(val), exc_tb
            encode = self.encoder
            if encode:
                result = Atom("e"), tuple(map(encode, e))
            else:
                result = Atom("e"), e
        return result


def start(port):
    global MessageHandler, call, set_encoder, set_decoder
    handler = MessageHandler(port)
    call = handler.call
    set_encoder = handler.set_encoder
    set_decoder = handler.set_decoder
    del MessageHandler
    handler.start()
