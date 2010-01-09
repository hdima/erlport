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

"""Erlang external term format.

See Erlang External Term Format for details:
    http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
"""

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

from struct import pack, unpack


class IncompleteData(ValueError):
    """Need more data."""

class Atom(str):
    """Erlang atom."""

    def __new__(cls, s):
        if len(s) > 255:
            raise ValueError("invalid atom length")
        return super(Atom, cls).__new__(cls, s)

    def __repr__(self):
        return "atom(%s)" % self


class String(unicode):
    """Erlang list/string wrapper."""

    def __new__(cls, s):
        if isinstance(s, list):
            # Raise TypeError
            s = u"".join(unichr(i) for i in s)
        elif not isinstance(s, unicode):
            raise TypeError("list or unicode object expected")
        return super(String, cls).__new__(cls, s)

    def __repr__(self):
        return "string(%s)" % super(String, self).__repr__()


class BitBinary(str):
    """Erlang bitstring whose length in bits is not a multiple of 8."""

    def __new__(cls, s, bits):
        obj = super(BitBinary, cls).__new__(cls, s)
        obj.bits = bits
        return obj

    def __repr__(self):
        return "bits(%s, %s)" % (self.bits, super(BitBinary, self).__repr__())


def decode(string):
    """Decode Erlang external term."""
    if len(string) < 1:
        raise IncompleteData("incomplete data: %r" % string)
    version = ord(string[0])
    if version != 131:
        raise ValueError("unknown protocol version: %i" % version)
    return decode_term(string[1:])

def decode_term(string):
    if len(string) < 1:
        raise IncompleteData("incomplete data: %r" % string)
    tag = ord(string[0])
    tail = string[1:]

    if tag == 97:
        if not tail:
            raise IncompleteData("incomplete data: %r" % string)
        return ord(tail[:1]), tail[1:]
    elif tag == 98:
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        i, = unpack(">i", tail[:4])
        return i, tail[4:]
    elif tag == 106:
        return [], tail
    elif tag == 107:
        if len(tail) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">H", tail[:2])
        tail = tail[2:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return [ord(i) for i in tail[:length]], tail[length:]
    elif tag == 108:
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">I", tail[:4])
        tail = tail[4:]
        lst = []
        while length > 0:
            term, tail = decode_term(tail)
            lst.append(term)
            length -= 1
        ignored, tail = decode_term(tail)
        return lst, tail
    elif tag == 109:
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">I", tail[:4])
        tail = tail[4:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return tail[:length], tail[length:]
    elif tag == 100:
        if len(tail) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">H", tail[:2])
        tail = tail[2:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return Atom(tail[:length]), tail[length:]
    elif tag == 104 or tag == 105:
        if tag == 104:
            if not tail:
                raise IncompleteData("incomplete data: %r" % string)
            arity = ord(tail[0])
            tail = tail[1:]
        else:
            if len(tail) < 4:
                raise IncompleteData("incomplete data: %r" % string)
            arity, = unpack(">I", tail[:4])
            tail = tail[4:]
        lst = []
        while arity > 0:
            term, tail = decode_term(tail)
            lst.append(term)
            arity -= 1
        return tuple(lst), tail
    elif tag == 70:
        term, = unpack(">d", tail[:8])
        return term, tail[8:]
    elif tag == 99:
        return float(tail[:31].split("\x00", 1)[0]), tail[31:]
    elif tag == 110 or tag == 111:
        if tag == 110:
            if len(tail) < 2:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = unpack(">BB", tail[:2])
            tail = tail[2:]
        else:
            if len(tail) < 5:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = unpack(">IB", tail[:5])
            tail = tail[5:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        n = sum(ord(c) << (8 * i) for i, c in enumerate(tail[:length]))
        if sign:
            n = -n
        return n, tail[length:]
    elif tag == 77:
        if len(tail) < 5:
            raise IncompleteData("incomplete data: %r" % string)
        length, bits = unpack(">IB", tail[:5])
        tail = tail[5:]
        if len(tail) < length:
            raise IncompleteData("incomplete daata: %r" % string)
        return BitBinary(tail[:length], bits), tail[length:]

    raise ValueError("unsupported data tag: %i" % tag)


def encode(term):
    """Encode Erlang external term."""
    return "\x83" + encode_term(term)

def encode_term(term):
    if isinstance(term, tuple):
        arity = len(term)
        if arity <= 255:
            header = 'h%c' % arity
        elif arity <= 4294967295:
            header = pack(">BI", 105, arity)
        else:
            raise ValueError("invalid tuple arity")
        return header + "".join(encode_term(t) for t in term)
    if isinstance(term, list):
        if not term:
            return "j"
        length = len(term)
        if length <= 65535:
            try:
                bytes = "".join(chr(i) for i in term if isinstance(i, int))
            except ValueError:
                pass
            else:
                if len(bytes) == length:
                    return pack(">BH", 107, length) + bytes
        elif length > 4294967295:
            raise ValueError("invalid list length")
        header = pack(">BI", 108, length)
        return header + "".join(encode_term(t) for t in term) + "j"
    elif isinstance(term, unicode):
        if not term:
            return "j"
        length = len(term)
        if length <= 65535:
            try:
                bytes = term.encode("latin1")
            except UnicodeEncodeError:
                pass
            else:
                return pack(">BH", 107, length) + bytes
        return encode_term([ord(i) for i in term])
    elif isinstance(term, Atom):
        return pack(">BH", 100, len(term)) + term
    elif isinstance(term, BitBinary):
        # Must be before str type
        return pack(">BIB", 77, len(term), term.bits) + term
    elif isinstance(term, str):
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid binary length")
        return pack(">BI", 109, length) + term
    elif isinstance(term, (int, long)):
        if 0 <= term <= 255:
            return 'a%c' % term
        elif -2147483648 <= term <= 2147483647:
            return pack(">Bi", 98, term)

        if term >= 0:
            sign = 0
        else:
            sign = 1
            term = -term

        bytes = []
        while term > 0:
            term, i = divmod(term, 256)
            bytes.append(chr(i))

        length = len(bytes)
        if length <= 255:
            return pack(">BBB", 110, length, sign) + "".join(bytes)
        elif length <= 4294967295:
            return pack(">BIB", 111, length, sign) + "".join(bytes)
        raise ValueError("invalid integer value")
    elif isinstance(term, float):
        return pack(">Bd", 70, term)

    raise ValueError("unsupported data type: %s" % type(term))
