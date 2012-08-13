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

from struct import pack, Struct
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
        elif not isinstance(s, str):
            raise TypeError("str object expected")
        elif len(s) > 255:
            raise ValueError("invalid atom length")
        return super(Atom, cls).__new__(cls, s)

    def __repr__(self):
        return "Atom(%s)" % super(Atom, self).__repr__()


class String(unicode):
    """Erlang list/string wrapper."""

    __slots__ = ()

    def __new__(cls, s):
        if isinstance(s, String):
            return s
        elif isinstance(s, list):
            # Will raise TypeError if can't be converted
            s = u"".join(map(unichr, s))
        elif not isinstance(s, unicode):
            raise TypeError("list or unicode object expected")
        return super(String, cls).__new__(cls, s)

    def __repr__(self):
        return "String(%s)" % super(String, self).__repr__()


class ImproperList(list):
    """Improper list."""

    __slots__ = "tail"

    def __init__(self, lst, tail):
        if not isinstance(lst, list):
            raise TypeError("list object expected")
        if isinstance(tail, list):
            raise TypeError("non list object expected for tail")
        self.tail = tail
        return super(ImproperList, self).__init__(lst)

    def __repr__(self):
        return "ImproperList(%s, %r)" % (
            super(ImproperList, self).__repr__(), self.tail)


class OpaqueObject(object):
    """Opaque object data."""

    __slots__ = "data", "language"

    marker = Atom("$opaque")

    def __init__(self, data, language):
        if not isinstance(data, str):
            raise TypeError("data must be instance of str")
        if not isinstance(language, Atom):
            raise TypeError("language must be instance of Atom")
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
        return "OpaqueObject(%r, %r)" % (self.data, self.language)


_python = Atom("python")

_big_int_unpack = Struct(">I").unpack
_small_int_unpack = Struct(">H").unpack
_big_signed_int_unpack = Struct(">i").unpack
_float_unpack = Struct(">d").unpack
_double_bytes_unpack = Struct("BB").unpack
_big_int_byte_unpack = Struct(">IB").unpack


def decode(string):
    """Decode Erlang external term."""
    if not string:
        raise IncompleteData("incomplete data: %r" % string)
    if string[0] != '\x83':
        raise ValueError("unknown protocol version: %r" % string[0])
    if string[1:2] == '\x50':
        # compressed term
        if len(string) < 6:
            raise IncompleteData("incomplete data: %r" % string)
        d = decompressobj()
        term_string = d.decompress(string[6:]) + d.flush()
        uncompressed_size, = _big_int_unpack(string[2:6])
        if len(term_string) != uncompressed_size:
            raise ValueError(
                "invalid compressed tag, "
                "%d bytes but got %d" % (uncompressed_size, len(term_string)))
        # tail data returned by decode_term() can be simple ignored
        term, _tail = decode_term(term_string)
        return term, d.unused_data
    return decode_term(string[1:])


def decode_term(string,
        # Hack to turn globals into locals
        len=len, ord=ord, tuple=tuple, float=float,
        big_int_unpack=_big_int_unpack, small_int_unpack=_small_int_unpack,
        big_signed_int_unpack=_big_signed_int_unpack,
        float_unpack=_float_unpack, double_bytes_unpack=_double_bytes_unpack,
        big_int_byte_unpack=_big_int_byte_unpack,
        Atom=Atom, opaque=OpaqueObject.marker):
    if not string:
        raise IncompleteData("incomplete data: %r" % string)
    tag = string[0]
    if tag == "d":
        # ATOM_EXT
        ln = len(string)
        if ln < 3:
            raise IncompleteData("incomplete data: %r" % string)
        length = small_int_unpack(string[1:3])[0] + 3
        if ln < length:
            raise IncompleteData("incomplete data: %r" % string)
        name = string[3:length]
        if name == "true":
            return True, string[length:]
        elif name == "false":
            return False, string[length:]
        elif name == "undefined":
            return None, string[length:]
        return Atom(name), string[length:]
    elif tag == "j":
        # NIL_EXT
        return [], string[1:]
    elif tag == "k":
        # STRING_EXT
        ln = len(string)
        if ln < 3:
            raise IncompleteData("incomplete data: %r" % string)
        length = small_int_unpack(string[1:3])[0] + 3
        if ln < length:
            raise IncompleteData("incomplete data: %r" % string)
        return array("B", string[3:length]).tolist(), string[length:]
    elif tag in "lhi":
        # LIST_EXT, SMALL_TUPLE_EXT, LARGE_TUPLE_EXT
        if tag == "h":
            if len(string) < 2:
                raise IncompleteData("incomplete data: %r" % string)
            length = ord(string[1])
            tail = string[2:]
        else:
            if len(string) < 5:
                raise IncompleteData("incomplete data: %r" % string)
            length, = big_int_unpack(string[1:5])
            tail = string[5:]
        lst = []
        append = lst.append
        _decode_term = decode_term
        while length > 0:
            term, tail = _decode_term(tail)
            append(term)
            length -= 1
        if tag == "l":
            if not tail:
                raise IncompleteData("incomplete data: %r" % string)
            if tail[0] != "j":
                improper_tail, tail = _decode_term(tail)
                return ImproperList(lst, improper_tail), tail
            return lst, tail[1:]
        if len(lst) == 3 and lst[0] == opaque:
            return OpaqueObject.decode(lst[2], lst[1]), tail
        return tuple(lst), tail
    elif tag == "a":
        # SMALL_INTEGER_EXT
        if len(string) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        return ord(string[1]), string[2:]
    elif tag == "b":
        # INTEGER_EXT
        if len(string) < 5:
            raise IncompleteData("incomplete data: %r" % string)
        i, = big_signed_int_unpack(string[1:5])
        return i, string[5:]
    elif tag == "m":
        # BINARY_EXT
        ln = len(string)
        if ln < 5:
            raise IncompleteData("incomplete data: %r" % string)
        length = big_int_unpack(string[1:5])[0] + 5
        if ln < length:
            raise IncompleteData("incomplete data: %r" % string)
        return string[5:length], string[length:]
    elif tag == "F":
        # NEW_FLOAT_EXT
        if len(string) < 9:
            raise IncompleteData("incomplete data: %r" % string)
        f, = float_unpack(string[1:9])
        return f, string[9:]
    elif tag in "no":
        # SMALL_BIG_EXT, LARGE_BIG_EXT
        if tag == "n":
            if len(string) < 3:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = double_bytes_unpack(string[1:3])
            tail = string[3:]
        else:
            if len(string) < 6:
                raise IncompleteData("incomplete data: %r" % string)
            length, sign = big_int_byte_unpack(string[1:6])
            tail = string[6:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        n = 0
        if length:
            for i in array("B", tail[length-1::-1]):
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
        is_improper = isinstance(term, ImproperList)
        if not term and not is_improper:
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
                    lst = pack(">cH", 'k', length) + bytes
                    if not is_improper:
                        return lst + "j"
                    return lst + encode_term(term.tail)
        elif length > 4294967295:
            raise ValueError("invalid list length")
        header = pack(">cI", 'l', length)
        lst = header + "".join(map(encode_term, term))
        if not is_improper:
            return lst + "j"
        return lst + encode_term(term.tail)
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
