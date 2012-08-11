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

"""Erlang external term format.

See Erlang External Term Format for details:
    http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
"""

__author__ = "Dmitry Vasiliev <dima@hlabs.org>"

from struct import pack, unpack
from array import array
from zlib import decompressobj, compress
from cPickle import loads, dumps, HIGHEST_PROTOCOL


class IncompleteData(ValueError):
    """Need more data."""


class Atom(str):
    """Erlang atom."""

    __slots__ = ()

    def __new__(cls, s):
        if isinstance(s, Atom):
            return s
        elif len(s) > 255:
            raise ValueError("invalid atom length")
        return super(Atom, cls).__new__(cls, s)

    def __repr__(self):
        return "atom(%s)" % super(Atom, self).__repr__()


class String(unicode):
    """Erlang list/string wrapper."""

    __slots__ = ()

    def __new__(cls, s):
        if isinstance(s, list):
            # Will raise TypeError if can't be converted
            s = u"".join(map(unichr, s))
        elif isinstance(s, String):
            return s
        elif not isinstance(s, unicode):
            raise TypeError("list or unicode object expected")
        return super(String, cls).__new__(cls, s)

    def __repr__(self):
        return "string(%s)" % super(String, self).__repr__()


class OpaqueObject(object):
    """Opaque object data."""

    __slots__ = "data", "language"

    marker = Atom("$opaque")

    def __init__(self, data, language):
        self.data = data
        self.language = language

    def decode(cls, data, language):
        if language == "python":
            return loads(data)
        return cls(data, language)
    decode = classmethod(decode)

    def encode(self):
        if self.language == "erlang":
            return self.data
        return encode_term((self.marker, self.language, self.data))

    def __eq__(self, other):
        return (type(self) == type(other) and self.language == other.language
            and self.data == other.data)

    def __repr__(self):
        return "opaque(%s)" % self.language


_python = Atom("python")


def decode(string):
    """Decode Erlang external term."""
    if not string:
        raise IncompleteData("incomplete data: %r" % string)
    if string[0] != '\x83':
        raise ValueError("unknown protocol version: %i" % string[0])
    if string[1:2] == '\x50':
        # compressed term
        if len(string) < 6:
            raise IncompleteData("incomplete data: %r" % string)
        d = decompressobj()
        term_string = d.decompress(string[6:]) + d.flush()
        uncompressed_size, = unpack('>I', string[2:6])
        if len(term_string) != uncompressed_size:
            raise ValueError(
                "invalid compressed tag, "
                "%d bytes but got %d" % (uncompressed_size, len(term_string)))
        # tail data returned by decode_term() can be simple ignored
        return decode_term(term_string)[0], d.unused_data
    return decode_term(string[1:])


def decode_term(string,
        # Hack to turn globals into locals
        len=len, ord=ord, unpack=unpack, tuple=tuple, float=float,
        Atom=Atom, opaque=OpaqueObject.marker):
    if not string:
        raise IncompleteData("incomplete data: %r" % string)
    tag = ord(string[0])
    tail = string[1:]
    if tag == 100:
        # ATOM_EXT
        if len(tail) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">H", tail[:2])
        tail = tail[2:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        name = tail[:length]
        tail = tail[length:]
        if name == "true":
            return True, tail
        elif name == "false":
            return False, tail
        elif name == "undefined":
            return None, tail
        return Atom(name), tail
    elif tag == 106:
        # NIL_EXT
        return [], tail
    elif tag == 107:
        # STRING_EXT
        if len(tail) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">H", tail[:2])
        tail = tail[2:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return array("B", tail[:length]).tolist(), tail[length:]
    elif tag == 108 or tag == 104 or tag == 105:
        # LIST_EXT, SMALL_TUPLE_EXT, LARGE_TUPLE_EXT
        if tag == 104:
            if not tail:
                raise IncompleteData("incomplete data: %r" % string)
            length = ord(tail[0])
            tail = tail[1:]
        else:
            if len(tail) < 4:
                raise IncompleteData("incomplete data: %r" % string)
            length, = unpack(">I", tail[:4])
            tail = tail[4:]
        lst = []
        append = lst.append
        _decode_term = decode_term
        while length > 0:
            term, tail = _decode_term(tail)
            append(term)
            length -= 1
        if tag == 108:
            _ignored, tail = _decode_term(tail)
            return lst, tail
        if len(lst) == 3 and lst[0] == opaque:
            return OpaqueObject.decode(lst[2], lst[1]), tail
        return tuple(lst), tail
    elif tag == 97:
        # SMALL_INTEGER_EXT
        if not tail:
            raise IncompleteData("incomplete data: %r" % string)
        return ord(tail[0]), tail[1:]
    elif tag == 98:
        # INTEGER_EXT
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        i, = unpack(">i", tail[:4])
        return i, tail[4:]
    elif tag == 109:
        # BINARY_EXT
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">I", tail[:4])
        tail = tail[4:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return tail[:length], tail[length:]
    elif tag == 70:
        # NEW_FLOAT_EXT
        if len(tail) < 8:
            raise IncompleteData("incomplete data: %r" % string)
        f, = unpack(">d", tail[:8])
        return f, tail[8:]
    elif tag == 110 or tag == 111:
        # SMALL_BIG_EXT, LARGE_BIG_EXT
        if tag == 110:
            if len(tail) < 2:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = unpack("BB", tail[:2])
            tail = tail[2:]
        else:
            if len(tail) < 5:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = unpack(">IB", tail[:5])
            tail = tail[5:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        n = 0
        for i in array('B', tail[length-1::-1]):
            n = (n << 8) | i
        if sign:
            n = -n
        return n, tail[length:]

    raise ValueError("unsupported data: %r" % (string,))


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
        Atom=Atom, str=str, float=float, ord=ord,
        dict=dict, True=True, False=False,
        ValueError=ValueError, OverflowError=OverflowError,
        python=_python):
    if isinstance(term, tuple):
        arity = len(term)
        if arity <= 255:
            header = 'h%c' % arity
        elif arity <= 4294967295:
            header = pack(">cI", 'i', arity)
        else:
            raise ValueError("invalid tuple arity")
        return header + "".join(map(encode_term, term))
    elif isinstance(term, list):
        length = len(term)
        if length == 0:
            return "j"
        elif length <= 65535:
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
                    return pack(">cH", 'k', length) + bytes
        elif length > 4294967295:
            raise ValueError("invalid list length")
        header = pack(">cI", 'l', length)
        return header + "".join(map(encode_term, term)) + "j"
    elif isinstance(term, unicode):
        length = len(term)
        if length == 0:
            return "j"
        elif length <= 65535:
            try:
                bytes = term.encode("latin1")
            except UnicodeEncodeError:
                pass
            else:
                return pack(">cH", 'k', length) + bytes
        return encode_term(map(ord, term))
    elif isinstance(term, Atom):
        return pack(">cH", 'd', len(term)) + term
    elif isinstance(term, str):
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid binary length")
        return pack(">cI", 'm', length) + term
    # Must be before int type
    elif term is True:
        return "\x64\x00\x04true"
    elif term is False:
        return "\x64\x00\x05false"
    elif isinstance(term, (int, long)):
        if 0 <= term <= 255:
            return 'a%c' % term
        elif -2147483648 <= term <= 2147483647:
            return pack(">ci", 'b', term)

        if term >= 0:
            sign = 0
        else:
            sign = 1
            term = -term

        bytes = array('B')
        while term:
            bytes.append(term & 0xff)
            term >>= 8

        length = len(bytes)
        if length <= 255:
            return pack("cBB", 'n', length, sign) + bytes.tostring()
        elif length <= 4294967295:
            return pack(">cIB", 'o', length, sign) + bytes.tostring()
        raise ValueError("invalid integer value")
    elif isinstance(term, float):
        return pack(">cd", 'F', term)
    elif isinstance(term, dict):
        # encode dict as proplist, but will be orddict compatible if keys
        # are all of the same type.
        items = term.items()
        # Faster than sorted(term.iteritems())
        items.sort()
        return encode_term(items)
    elif term is None:
        return "\x64\x00\x09undefined"
    elif isinstance(term, OpaqueObject):
        return term.encode()

    try:
        data = dumps(term, HIGHEST_PROTOCOL)
    except:
        raise ValueError("unsupported data type: %s" % type(term))
    return OpaqueObject(data, python).encode()
