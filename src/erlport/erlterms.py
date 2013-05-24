# Copyright (c) 2009, 2010, Dmitry Vasiliev <dima@hlabs.org>
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

__author__ = "Dmitry Vasiliev <dima@hlabs.org>"

from struct import pack, unpack
from itertools import islice
from array import array
from zlib import decompressobj, compress
from datetime import datetime


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
    if string[1:2] == '\x50':
        # compressed term
        if len(string) < 6:
            raise IncompleteData("incomplete data: %r" % string)
        d = decompressobj()
        zlib_data = string[6:]
        term_string = d.decompress(zlib_data) + d.flush()
        uncompressed_size = unpack('>I', string[2:6])[0]
        if len(term_string) != uncompressed_size:
            raise ValueError(
                "invalid compressed tag, "
                "%d bytes but got %d" % (uncompressed_size, len(term_string)))
        # tail data returned by decode_term() can be simple ignored
        return decode_term(iter(term_string))
    it = iter(string)
    it.next()
    return decode_term(it)


def decode_term(it,
                # Hack to turn globals into locals
                len=len, ord=ord, unpack=unpack, tuple=tuple, float=float,
                BitBinary=BitBinary, Atom=Atom):
    try:
        tag = ord(it.next())
        if tag == 97:
            # SMALL_INTEGER_EXT
            return ord(it.next())
        elif tag == 98:
            # INTEGER_EXT
            i, = unpack(">i", ''.join(islice(it, 0, 4)))
            return i
        elif tag == 106:
            # NIL_EXT
            return []
        elif tag == 107:
            # STRING_EXT
            length, = unpack(">H", ''.join(islice(it, 0, 2)))
            string = ''.join(islice(it, 0, length))
            return [ord(i) for i in string]
        elif tag == 108:
            # LIST_EXT
            length, = unpack(">I", ''.join(islice(it, 0, 4)))
            lst = []
            while length > 0:
                term = decode_term(it)
                lst.append(term)
                length -= 1
            return lst
        elif tag == 109:
            # BINARY_EXT
            length, = unpack(">I", ''.join(islice(it, 0, 4)))
            return ''.join(islice(it, 0, length))
        elif tag == 100:
            # ATOM_EXT
            length, = unpack(">H", ''.join(islice(it, 0, 2)))
            name = ''.join(islice(it, 0, length))
            if name == "true":
                return True
            elif name == "false":
                return False
            elif name == "none":
                return None
            return Atom(''.join(name))
        elif tag == 104 or tag == 105:
            # SMALL_TUPLE_EXT, LARGE_TUPLE_EXT
            if tag == 104:
                arity = ord(it.next())
            else:
                arity, = unpack(">I", ''.join(islice(it, 0, 4)))
            lst = []
            while arity > 0:
                term = decode_term(it)
                lst.append(term)
                arity -= 1
            return tuple(lst)
        elif tag == 70:
            # NEW_FLOAT_EXT
            term, = unpack(">d", ''.join(islice(it, 0, 8)))
            return term
        elif tag == 99:
            # FLOAT_EXT
            return float(''.join(islice(it, 0, 31)).split("\x00", 1)[0])
        elif tag == 110 or tag == 111:
            # SMALL_BIG_EXT, LARGE_BIG_EXT
            if tag == 110:
                length, sign = unpack(">BB", ''.join(islice(it, 0, 2)))
            else:
                length, sign = unpack(">IB", ''.join(islice(it, 0, 5)))
            n = 0
            l = ''.join(islice(it, 0, length-1))
            l.reverse()
            for i in array('B', l):
                n = (n << 8) | i
            if sign:
                n = -n
            return n
        elif tag == 77:
            # BIT_BINARY_EXT
            length, bits = unpack(">IB", ''.join(islice(it, 0, 5)))
            return BitBinary(''.join(islice(it, 0, 5)), bits)
    except StopIteration :
        raise ValueError("unsupported data tag: %i" % list(it))

    raise ValueError("unsupported data tag: %i" % tag)


def encode(term, compressed=False):
    """Encode Erlang external term."""
    encoded_term = encode_term(term)
    # False and 0 do not attempt compression.
    if compressed:
        if compressed is True:
            # default compression level of 6
            compressed = 6
        zlib_term = compress(encoded_term, compressed)
        if len(zlib_term) + 5 <= len(encoded_term):
            # compressed term is smaller
            return '\x83\x50' + pack('>I', len(encoded_term)) + zlib_term
    return "\x83" + encoded_term


def encode_term(term,
                # Hack to turn globals into locals
                pack=pack, tuple=tuple, len=len, isinstance=isinstance,
                list=list, int=int, long=long, array=array, unicode=unicode,
                Atom=Atom, BitBinary=BitBinary, str=str, float=float, ord=ord,
                dict=dict, datetime=datetime, True=True, False=False,
                ValueError=ValueError, OverflowError=OverflowError):
    if isinstance(term, tuple):
        arity = len(term)
        if arity <= 255:
            header = 'h%c' % arity
        elif arity <= 4294967295:
            header = pack(">BI", 105, arity)
        else:
            raise ValueError("invalid tuple arity")
        _encode_term = encode_term
        return header + "".join(_encode_term(t) for t in term)
    if isinstance(term, list):
        if not term:
            return "j"
        length = len(term)
        if length <= 65535:
            try:
                # array coersion will allow floats as a deprecated feature
                for t in term:
                    if not isinstance(t, (int, long)):
                        raise TypeError
                bytes = array('B', term).tostring()
            except (TypeError, OverflowError):
                pass
            else:
                if len(bytes) == length:
                    return pack(">BH", 107, length) + bytes
        elif length > 4294967295:
            raise ValueError("invalid list length")
        header = pack(">BI", 108, length)
        _encode_term = encode_term
        return header + "".join(_encode_term(t) for t in term) + "j"
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
    # must be before int type
    elif term is True or term is False:
        term = term and 'true' or 'false'
        return pack(">BH", 100, len(term)) + term
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

        bytes = array('B')
        while term > 0:
            bytes.append(term & 0xff)
            term >>= 8

        length = len(bytes)
        if length <= 255:
            return pack(">BBB", 110, length, sign) + bytes.tostring()
        elif length <= 4294967295:
            return pack(">BIB", 111, length, sign) + bytes.tostring()
        raise ValueError("invalid integer value")
    elif isinstance(term, float):
        return pack(">Bd", 70, term)
    elif isinstance(term, dict):
        # encode dict as proplist, but will be orddict compatible if keys
        # are all of the same type.
        return encode_term(sorted(term.iteritems()))
    elif term is None:
        return pack(">BH", 100, 4) + "none"
    elif isinstance(term, datetime):
        return encode_term(((term.year, term.month, term.day),
            (term.hour, term.minute, term.second)))

    raise ValueError("unsupported data type: %s" % type(term))
