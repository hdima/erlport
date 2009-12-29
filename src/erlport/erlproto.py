#
# Copyrigh (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
#
"""Erlang port protocol."""

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

import os
import errno
from struct import pack, unpack

from erlport.erlterms import Atom, encode, decode


class PortProtocol(object):
    """Erlang port protocol."""

    _formats = {
        1: "B",
        2: ">H",
        4: ">I",
        }

    def __init__(self, processor, packet=1, use_stdio=False):
        self._format = self._formats.get(packet)
        if self._format is None:
            raise ValueError("invalid packet size value: %s" % packet)
        self.packet = packet

        if use_stdio:
            self._fin = os.fdopen(0, "rb")
            self._fout = os.fdopen(1, "wb")
        else:
            self._fin = os.fdopen(3, "rb")
            self._fout = os.fdopen(4, "wb")

        self.processor = processor

    def start(self):
        """Start message processing."""
        while True:
            try:
                self.handle_message()
            except EOFError:
                break

    def handle_message(self):
        """Handle incoming message."""
        request = self.read_message()
        handler = getattr(self.processor, request[0], None)
        if handler is None:
            reponse = Atom("error"), Atom("undef")
        else:
            try:
                response = handler(*request[1:])
            except TypeError:
                # FIXME: It's better to check inspect.getargspec() result
                response = Atom("error"), Atom("function_clause")
        self.write_result(response)

    def read_message(self):
        """Read incoming message."""
        data = self._fin.read(self.packet)
        if not data:
            raise EOFError()
        length, = unpack(self._format, data)
        data = self._fin.read(length)
        if not data:
            raise EOFError()
        return decode(data)[0]

    def write_result(self, response):
        """Write outgoing result."""
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
        """Close protocol."""
        self._fin.close()
        self._fout.close()
