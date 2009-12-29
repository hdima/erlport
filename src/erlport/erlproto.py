#
# Copyrigh (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
#

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

import os
import errno
from struct import pack, unpack

from erlport.erlterms import Atom, encode, decode


class PortProtocol(object):

    def __init__(self, processor):
        self._fin = os.fdopen(3, "rb")
        self._fout = os.fdopen(4, "wb")
        self.processor = processor

    def read_message(self):
        data = self._fin.read(4)
        if not data:
            raise EOFError()
        length, = unpack(">I", data)
        data = self._fin.read(length)
        if not data:
            raise EOFError()
        return decode(data)[0]

    def start(self):
        while True:
            try:
                self.handle_message()
            except EOFError:
                break

    def handle_message(self):
        request = self.read_message()
        handler = getattr(self.processor, request[0], None)
        if handler is None:
            reponse = Atom("error"), Atom("badarg")
        else:
            try:
                response = handler(*request[1:])
            except TypeError:
                response = Atom("error"), Atom("badarg")
        self.write_result(response)

    def write_result(self, response):
        data = encode(response)
        data = pack(">I", len(data)) + data
        try:
            self._fout.write(data)
            self._fout.flush()
        except IOError, why:
            if why.errno == errno.EPIPE:
                raise EOFError()
            raise

    def close(self):
        self._fin.close()
        self._fout.close()
