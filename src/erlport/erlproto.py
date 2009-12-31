# Copyright (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
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

"""Erlang port protocol."""

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

import os
import errno
from struct import pack, unpack

from erlport.erlterms import Atom, encode, decode


class Protocol(object):
    """Erlang port protocol."""

    def connected(self, port):
        """Port connected."""
        self.port = port

    def disconnected(self):
        """Port disconnected."""

    def handle(self, message):
        """Handle incoming message."""
        if not (isinstance(message, tuple) and len(message) > 0):
            response = Atom("error"), Atom("badarg")
        else:
            name = message.pop(0)
            if not isinstance(name, Atom):
                response = Atom("error"), Atom("badarg")
            else:
                handler = getattr(self, "handle_%s" % name, None)
                if handler is None:
                    response = Atom("error"), Atom("undef")
                else:
                    try:
                        response = handler(*message)
                    except TypeError:
                        # Easy way to check correct number of arguments
                        response = Atom("error"), Atom("function_clause")
        self.port.write(response)


class Port(object):
    """Erlang port."""

    _formats = {
        1: "B",
        2: ">H",
        4: ">I",
        }

    def __init__(self, proto):
        self.proto = proto
        self._connected = False

    def _init(self, packet, use_stdio):
        """Initialize protocol details."""
        self._format = self._formats.get(packet)
        if self._format is None:
            raise ValueError("invalid packet size value: %s" % packet)
        self.packet = packet

        if use_stdio:
            in_d, out_d = 0, 1
        else:
            in_d, out_d = 3, 4
        self._fin = os.fdopen(in_d, "rb")
        self._fout = os.fdopen(out_d, "wb")

    def connect(self, packet=1, use_stdio=False):
        """Connect port and start message processing."""
        if self._connected:
            raise RuntimeError("already connected")
        self._connected = True
        try:
            self._init(packet, use_stdio)
            self.proto.connected(self)
            while self._connected:
                try:
                    message = self.read()
                    self.proto.handle(message)
                except EOFError:
                    break
        finally:
            self._connected = False
            self.close()
            self.proto.disconnected()

    def _read(self, length):
        try:
            data = self._fin.read(length)
        except IOError, why:
            if why.errno == errno.EPIPE:
                raise EOFError()
            raise
        if not data:
            raise EOFError()
        return data

    def read(self):
        """Read incoming message."""
        data = self._read(self.packet)
        length, = unpack(self._format, data)
        data = self._read(length)
        return decode(data)[0]

    def write(self, response):
        """Write outgoing message."""
        data = encode(response)
        data = pack(self._format, len(data)) + data
        try:
            self._fout.write(data)
            self._fout.flush()
        except IOError, why:
            if why.errno == errno.EPIPE:
                raise EOFError()
            raise

    def close(self):
        self._connected = False
        self._fin.close()
        self._fout.close()
