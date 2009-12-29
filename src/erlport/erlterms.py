#
# Copyrigh (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
#

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

from struct import pack, unpack

#
# TODO:
# - Add dictionaries, floats, big integer...
#


class IncompleteData(ValueError):
    pass

class Atom(str):

    def __new__(cls, s):
        if len(s) > 255:
            raise ValueError("invalid atom length")
        return str.__new__(cls, s)

    def __repr__(self):
        return "atom(%s)" % self


class String(unicode):

    def getList(self):
        return [ord(i) for i in self]

    def __repr__(self):
        return "string(%s)" % unicode.__repr__(self)


def decode(string):
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
        i, = unpack(">I", tail[:4])
        return i, tail[4:]
    elif tag == 106:
        return String(""), tail
    elif tag == 107:
        if len(tail) < 2:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">H", tail[:2])
        tail = tail[2:]
        if len(tail) < length:
            raise IncompleteData("incomplete data: %r" % string)
        return String(tail[:length]), tail[length:]
    elif tag == 108:
        if len(tail) < 4:
            raise IncompleteData("incomplete data: %r" % string)
        length, = unpack(">I", tail[:4])
        tail = tail[4:]
        lst = []
        is_string = True
        while length > 0:
            term, tail = decode_term(tail)
            if is_string and not isinstance(term, int):
                is_string = False
            lst.append(term)
            length -= 1
        term, tail = decode_term(tail)
        if is_string:
            return String(u"".join(unichr(i) for i in lst)), tail
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

    raise ValueError("unsupported data tag: %i" % tag)


def encode(term):
    return "\x83" + encode_term(term)

def encode_term(term):
    if isinstance(term, tuple):
        arity = len(term)
        if arity <= 255:
            header = pack("BB", 104, arity)
        elif arity <= 4294967295:
            header = pack(">BI", 105, arity)
        else:
            raise ValueError("invalid tuple arity")
        return header + "".join(encode_term(t) for t in term)
    if isinstance(term, list):
        if not term:
            return "j"
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid list length")
        header = pack(">BI", 108, length)
        return header + "".join(encode_term(t) for t in term) + "j"
    elif isinstance(term, unicode):
        if not term:
            return "j"
        length = len(term)
        if length <= 65535:
            for i in term:
                if ord(i) > 255:
                    break
            else:
                return pack(">BH", 107, length) + str(term)
        return encode_term([ord(i) for i in term])
    elif isinstance(term, Atom):
        return pack(">BH", 100, len(term)) + term
    elif isinstance(term, str):
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid binary length")
        return pack(">BI", 109, length) + term
    elif isinstance(term, (int, long)):
        if term > 4294967295:
            raise ValueError("invalid integer value")
        elif term <= 255:
            return pack(">BB", 97, term)
        return pack(">BI", 98, term)

    raise ValueError("unsupported data type: %s" % type(term))
