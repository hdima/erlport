# Copyright (c) 2009, 2010, Dmitry Vasiliev <dima@hlabs.spb.ru>
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

    def run(self, port):
        """Run processing loop."""
        while True:
            try:
                message = port.read()
            except EOFError:
                break
            self.handle(port, message)

    def handle(self, port, message):
        """Handle incoming message."""
        if not (isinstance(message, Atom)
                or isinstance(message, tuple) and len(message) > 0):
            response = Atom("error"), Atom("badarg")
        else:
            if isinstance(message, Atom):
                name = message
                args = ()
            else:
                name = message[0]
                args = message[1:]
            if not isinstance(name, Atom):
                response = Atom("error"), Atom("badarg")
            else:
                handler = getattr(self, "handle_%s" % name, None)
                if handler is None:
                    response = Atom("error"), Atom("undef")
                else:
                    try:
                        response = handler(*args)
                    except TypeError:
                        # Easy way to check correct number of arguments
                        response = Atom("error"), Atom("function_clause")
        port.write(response)


class Port(object):
    """Erlang port."""

    _formats = {
        1: "B",
        2: ">H",
        4: ">I",
        }

    def __init__(self, packet=1, use_stdio=False, compressed=False,
            descriptors=None):
        self._format = self._formats.get(packet)
        if self._format is None:
            raise ValueError("invalid packet size value: %s" % packet)
        self.packet = packet
        self.compressed = compressed

        if descriptors is not None:
            self.in_d, self.out_d = descriptors
        elif use_stdio:
            self.in_d, self.out_d = 0, 1
        else:
            self.in_d, self.out_d = 3, 4

    def _read_data(self, length):
        data = ""
        while len(data) != length:
            try:
                buf = os.read(self.in_d, length)
            except IOError, why:
                if why.errno == errno.EPIPE:
                    raise EOFError()
                raise
            if not buf:
                raise EOFError()
            data += buf
        return data

    def read(self):
        """Read incoming message."""
        data = self._read_data(self.packet)
        length, = unpack(self._format, data)
        data = self._read_data(length)
        return decode(data)[0]

    def write(self, message):
        """Write outgoing message."""
        data = encode(message, compressed=self.compressed)
        data = pack(self._format, len(data)) + data
        length = len(data)
        if length != 0:
            try:
                n = os.write(self.out_d, data)
            except IOError, why:
                if why.errno == errno.EPIPE:
                    raise EOFError()
                raise
            if n == 0:
                raise EOFError()
            length -= n

    def close(self):
        """Close port."""
        os.close(self.in_d)
        os.close(self.out_d)
