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

import unittest

from pickle import dumps

from erlport import erlterms
from erlport.erlterms import Atom, String, ImproperList, OpaqueObject
from erlport.erlterms import encode, decode, IncompleteData


class AtomTestCase(unittest.TestCase):

    def test_atom(self):
        atom = Atom("test")
        self.assertEqual(Atom, type(atom))
        self.assertEqual("test", atom)
        self.assertEqual("Atom('test')", repr(atom))
        self.assertTrue(atom is Atom(atom))
        self.assertEqual("X" * 255, Atom("X" * 255))
        self.assertRaises(ValueError, Atom, "X" * 256)
        self.assertRaises(TypeError, Atom, [1, 2])

class StringTestCase(unittest.TestCase):

    def test_string(self):
        string = String(u"test")
        self.assertEqual(String, type(string))
        self.assertEqual(u"test", string)
        self.assertEqual("String(u'test')", repr(string))
        self.assertTrue(string is String(string))
        self.assertEqual(u"test", String([116, 101, 115, 116]))
        self.assertRaises(TypeError, String, (1, 2))
        self.assertRaises(TypeError, String, ["a", "b"])

class ImproperListTestCase(unittest.TestCase):

    def test_improper_list(self):
        improper = ImproperList([1, 2, 3], "tail")
        self.assertEqual(ImproperList, type(improper))
        self.assertEqual([1, 2, 3], improper)
        self.assertEqual("tail", improper.tail)
        self.assertEqual("ImproperList([1, 2, 3], 'tail')", repr(improper))
        self.assertRaises(TypeError, ImproperList, "invalid", "tail")
        self.assertRaises(TypeError, ImproperList, [1, 2, 3], ["invalid"])
        self.assertRaises(ValueError, ImproperList, [], "tail")

class OpaqueObjectTestCase(unittest.TestCase):

    def test_opaque_object(self):
        obj = OpaqueObject("data", Atom("language"))
        self.assertEqual(OpaqueObject, type(obj))
        self.assertEqual("data", obj.data)
        self.assertEqual("language", obj.language)
        self.assertEqual("OpaqueObject('data', Atom('language'))", repr(obj))
        self.assertRaises(TypeError, OpaqueObject, "data", "language")
        self.assertRaises(TypeError, OpaqueObject, [1, 2], Atom("language"))

    def test_comparison(self):
        obj = OpaqueObject("data", Atom("language"))
        self.assertEqual(obj, obj)
        self.assertEqual(OpaqueObject("data", Atom("language")), obj)
        self.assertNotEqual(OpaqueObject("data", Atom("language2")), obj)

    def test_decode(self):
        obj = OpaqueObject.decode("data", Atom("language"))
        self.assertEqual("data", obj.data)
        self.assertEqual("language", obj.language)

    def test_decode_python(self):
        data = OpaqueObject.decode(dumps("test"), Atom("python"))
        self.assertEqual("test", data)

    def test_encode(self):
        obj = OpaqueObject("data", Atom("language"))
        term = Atom("$opaque"), Atom("language"), "data"
        self.assertEqual(erlterms.encode_term(term), obj.encode())

    def test_encode_erlang(self):
        obj = OpaqueObject("data", Atom("erlang"))
        self.assertEqual("data", obj.encode())

class DecodeTestCase(unittest.TestCase):

    def test_decode(self):
        self.assertRaises(IncompleteData, decode, "")
        self.assertRaises(ValueError, decode, "\0")
        self.assertRaises(IncompleteData, decode, "\x83")
        self.assertRaises(ValueError, decode, "\x83z")

    def test_decode_atom(self):
        self.assertRaises(IncompleteData, decode, "\x83d")
        self.assertRaises(IncompleteData, decode, "\x83d\0")
        self.assertRaises(IncompleteData, decode, "\x83d\0\1")
        self.assertEqual((Atom(""), ""), decode("\x83d\0\0"))
        self.assertEqual((Atom(""), "tail"), decode("\x83d\0\0tail"))
        self.assertEqual((Atom("test"), ""), decode("\x83d\0\4test"))
        self.assertEqual((Atom("test"), "tail"), decode("\x83d\0\4testtail"))

    def test_decode_predefined_atoms(self):
        self.assertEqual((True, ""), decode("\x83d\0\4true"))
        self.assertEqual((False, ""), decode("\x83d\0\5false"))
        self.assertEqual((None, ""), decode("\x83d\0\11undefined"))

    def test_decode_empty_list(self):
        self.assertEqual(([], ""), decode("\x83j"))
        self.assertEqual(([], "tail"), decode("\x83jtail"))

    def test_decode_string_list(self):
        self.assertRaises(IncompleteData, decode, "\x83k")
        self.assertRaises(IncompleteData, decode, "\x83k\0")
        self.assertRaises(IncompleteData, decode, "\x83k\0\1")
        # Erlang use 'j' tag for empty lists
        self.assertEqual(([], ""), decode("\x83k\0\0"))
        self.assertEqual(([], "tail"), decode("\x83k\0\0tail"))
        self.assertEqual(([116, 101, 115, 116], ""), decode("\x83k\0\4test"))
        self.assertEqual(([116, 101, 115, 116], "tail"),
            decode("\x83k\0\4testtail"))

    def test_decode_list(self):
        self.assertRaises(IncompleteData, decode, "\x83l")
        self.assertRaises(IncompleteData, decode, "\x83l\0")
        self.assertRaises(IncompleteData, decode, "\x83l\0\0")
        self.assertRaises(IncompleteData, decode, "\x83l\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83l\0\0\0\0")
        # Elang use 'j' tag for empty lists
        self.assertEqual(([], ""), decode("\x83l\0\0\0\0j"))
        self.assertEqual(([], "tail"), decode("\x83l\0\0\0\0jtail"))
        self.assertEqual(([[], []], ""), decode("\x83l\0\0\0\2jjj"))
        self.assertEqual(([[], []], "tail"), decode("\x83l\0\0\0\2jjjtail"))

    def test_decode_improper_list(self):
        self.assertRaises(IncompleteData, decode, "\x83l\0\0\0\0k")
        improper, tail = decode("\x83l\0\0\0\1jd\0\4tail")
        self.assertEqual(ImproperList, type(improper))
        self.assertEqual([[]], improper)
        self.assertEqual(Atom("tail"), improper.tail)
        self.assertEqual("", tail)
        improper, tail = decode("\x83l\0\0\0\1jd\0\4tailtail")
        self.assertEqual(ImproperList, type(improper))
        self.assertEqual([[]], improper)
        self.assertEqual(Atom("tail"), improper.tail)
        self.assertEqual("tail", tail)

    def test_decode_small_tuple(self):
        self.assertRaises(IncompleteData, decode, "\x83h")
        self.assertRaises(IncompleteData, decode, "\x83h\1")
        self.assertEqual(((), ""), decode("\x83h\0"))
        self.assertEqual(((), "tail"), decode("\x83h\0tail"))
        self.assertEqual((([], []), ""), decode("\x83h\2jj"))
        self.assertEqual((([], []), "tail"), decode("\x83h\2jjtail"))

    def test_decode_large_tuple(self):
        self.assertRaises(IncompleteData, decode, "\x83i")
        self.assertRaises(IncompleteData, decode, "\x83i\0")
        self.assertRaises(IncompleteData, decode, "\x83i\0\0")
        self.assertRaises(IncompleteData, decode, "\x83i\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83i\0\0\0\1")
        # Erlang use 'h' tag for small tuples
        self.assertEqual(((), ""), decode("\x83i\0\0\0\0"))
        self.assertEqual(((), "tail"), decode("\x83i\0\0\0\0tail"))
        self.assertEqual((([], []), ""), decode("\x83i\0\0\0\2jj"))
        self.assertEqual((([], []), "tail"), decode("\x83i\0\0\0\2jjtail"))

    def test_decode_opaque_object(self):
        opaque, tail = decode("\x83h\3d\0\7$opaqued\0\10languagem\0\0\0\4data")
        self.assertEqual(OpaqueObject, type(opaque))
        self.assertEqual("data", opaque.data)
        self.assertEqual("language", opaque.language)
        self.assertEqual("", tail)
        opaque, tail = decode("\x83h\3d\0\7$opaqued\0\10language"
            "m\0\0\0\4datatail")
        self.assertEqual(OpaqueObject, type(opaque))
        self.assertEqual("data", opaque.data)
        self.assertEqual("language", opaque.language)
        self.assertEqual("tail", tail)

    def test_decode_python_opaque_object(self):
        data, tail = decode("\x83h\3d\0\7$opaqued\0\6python"
            "m\0\0\0\14S'test'\np0\n.")
        self.assertEqual("test", data)
        self.assertEqual("", tail)
        data, tail = decode("\x83h\3d\0\7$opaqued\0\6python"
            "m\0\0\0\14S'test'\np0\n.tail")
        self.assertEqual("test", data)
        self.assertEqual("tail", tail)

    def test_decode_small_integer(self):
        self.assertRaises(IncompleteData, decode, "\x83a")
        self.assertEqual((0, ""), decode("\x83a\0"))
        self.assertEqual((0, "tail"), decode("\x83a\0tail"))
        self.assertEqual((255, ""), decode("\x83a\xff"))
        self.assertEqual((255, "tail"), decode("\x83a\xfftail"))

    def test_decode_integer(self):
        self.assertRaises(IncompleteData, decode, "\x83b")
        self.assertRaises(IncompleteData, decode, "\x83b\0")
        self.assertRaises(IncompleteData, decode, "\x83b\0\0")
        self.assertRaises(IncompleteData, decode, "\x83b\0\0\0")
        # Erlang use 'a' tag for small integers
        self.assertEqual((0, ""), decode("\x83b\0\0\0\0"))
        self.assertEqual((0, "tail"), decode("\x83b\0\0\0\0tail"))
        self.assertEqual((2147483647, ""), decode("\x83b\x7f\xff\xff\xff"))
        self.assertEqual((2147483647, "tail"),
            decode("\x83b\x7f\xff\xff\xfftail"))
        self.assertEqual((-1, ""), decode("\x83b\xff\xff\xff\xff"))
        self.assertEqual((-1, "tail"), decode("\x83b\xff\xff\xff\xfftail"))

    def test_decode_binary(self):
        self.assertRaises(IncompleteData, decode, "\x83m")
        self.assertRaises(IncompleteData, decode, "\x83m\0")
        self.assertRaises(IncompleteData, decode, "\x83m\0\0")
        self.assertRaises(IncompleteData, decode, "\x83m\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83m\0\0\0\1")
        self.assertEqual(("", ""), decode("\x83m\0\0\0\0"))
        self.assertEqual(("", "tail"), decode("\x83m\0\0\0\0tail"))
        self.assertEqual(("data", ""), decode("\x83m\0\0\0\4data"))
        self.assertEqual(("data", "tail"), decode("\x83m\0\0\0\4datatail"))

    def test_decode_float(self):
        self.assertRaises(IncompleteData, decode, "\x83F")
        self.assertRaises(IncompleteData, decode, "\x83F\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0\0\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83F\0\0\0\0\0\0\0")
        self.assertEqual((0.0, ""), decode("\x83F\0\0\0\0\0\0\0\0"))
        self.assertEqual((0.0, "tail"), decode("\x83F\0\0\0\0\0\0\0\0tail"))
        self.assertEqual((1.5, ""), decode("\x83F?\xf8\0\0\0\0\0\0"))
        self.assertEqual((1.5, "tail"), decode("\x83F?\xf8\0\0\0\0\0\0tail"))

    def test_decode_small_big_integer(self):
        self.assertRaises(IncompleteData, decode, "\x83n")
        self.assertRaises(IncompleteData, decode, "\x83n\0")
        self.assertRaises(IncompleteData, decode, "\x83n\1\0")
        # Erlang use 'a' tag for small integers
        self.assertEqual((0, ""), decode("\x83n\0\0"))
        self.assertEqual((0, "tail"), decode("\x83n\0\0tail"))
        self.assertEqual((6618611909121, ""), decode("\x83n\6\0\1\2\3\4\5\6"))
        self.assertEqual((-6618611909121, ""), decode("\x83n\6\1\1\2\3\4\5\6"))
        self.assertEqual((6618611909121, "tail"),
            decode("\x83n\6\0\1\2\3\4\5\6tail"))

    def test_decode_big_integer(self):
        self.assertRaises(IncompleteData, decode, "\x83o")
        self.assertRaises(IncompleteData, decode, "\x83o\0")
        self.assertRaises(IncompleteData, decode, "\x83o\0\0")
        self.assertRaises(IncompleteData, decode, "\x83o\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83o\0\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83o\0\0\0\1\0")
        # Erlang use 'a' tag for small integers
        self.assertEqual((0, ""), decode("\x83o\0\0\0\0\0"))
        self.assertEqual((0, "tail"), decode("\x83o\0\0\0\0\0tail"))
        self.assertEqual((6618611909121, ""),
            decode("\x83o\0\0\0\6\0\1\2\3\4\5\6"))
        self.assertEqual((-6618611909121, ""),
            decode("\x83o\0\0\0\6\1\1\2\3\4\5\6"))
        self.assertEqual((6618611909121, "tail"),
            decode("\x83o\0\0\0\6\0\1\2\3\4\5\6tail"))

    def test_decode_compressed_term(self):
        self.assertRaises(IncompleteData, decode, "\x83P")
        self.assertRaises(IncompleteData, decode, "\x83P\0")
        self.assertRaises(IncompleteData, decode, "\x83P\0\0")
        self.assertRaises(IncompleteData, decode, "\x83P\0\0\0")
        self.assertRaises(IncompleteData, decode, "\x83P\0\0\0\0")
        self.assertRaises(ValueError, decode, "\x83P\0\0\0\x16"
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50")
        self.assertEqual(([100] * 20, ""), decode("\x83P\0\0\0\x17"
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50"))
        self.assertEqual(([100] * 20, "tail"), decode("\x83P\0\0\0\x17"
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50tail"))

class EncodeTestCase(unittest.TestCase):

    def test_encode_tuple(self):
        self.assertEqual("\x83h\0", encode(()))
        self.assertEqual("\x83h\2h\0h\0", encode(((), ())))
        self.assertEqual("\x83h\xff" + "h\0" * 255, encode(tuple([()] * 255)))
        self.assertEqual("\x83i\0\0\1\0" + "h\0" * 256,
            encode(tuple([()] * 256)))

    def test_encode_empty_list(self):
        self.assertEqual("\x83j", encode([]))

    def test_encode_string_list(self):
        self.assertEqual("\x83k\0\1\0j", encode([0]))
        r = range(0, 256)
        self.assertEqual("\x83k\1\0" + "".join(map(chr, r)) + "j", encode(r))

    def test_encode_list(self):
        self.assertEqual("\x83l\0\0\0\1jj", encode([[]]))
        self.assertEqual("\x83l\0\0\0\5jjjjjj", encode([[], [], [], [], []]))

    def test_encode_improper_list(self):
        self.assertEqual("\x83l\0\0\0\1h\0h\0", encode(ImproperList([()], ())))

    def test_encode_unicode(self):
        self.assertEqual("\x83j", encode(u""))
        self.assertEqual("\x83k\0\4testj", encode(u"test"))
        self.assertEqual("\x83l\0\0\0\4b\0\0\4Bb\0\0\x045b\0\0\4Ab\0\0\4Bj",
            encode(u"\u0442\u0435\u0441\u0442"))

    def test_encode_atom(self):
        self.assertEqual("\x83d\0\0", encode(Atom("")))
        self.assertEqual("\x83d\0\4test", encode(Atom("test")))

    def test_encode_string(self):
        self.assertEqual("\x83m\0\0\0\0", encode(""))
        self.assertEqual("\x83m\0\0\0\4test", encode("test"))

    def test_encode_boolean(self):
        self.assertEqual("\x83d\0\4true", encode(True))
        self.assertEqual("\x83d\0\5false", encode(False))

    def test_encode_none(self):
        self.assertEqual("\x83d\0\11undefined", encode(None))

    def test_encode_short_integer(self):
        self.assertEqual("\x83a\0", encode(0))
        self.assertEqual("\x83a\xff", encode(255))

    def test_encode_integer(self):
        self.assertEqual("\x83b\xff\xff\xff\xff", encode(-1))
        self.assertEqual("\x83b\x80\0\0\0", encode(-2147483648))
        self.assertEqual("\x83b\0\0\1\0", encode(256))
        self.assertEqual("\x83b\x7f\xff\xff\xff", encode(2147483647))

    def test_encode_long_integer(self):
        self.assertEqual("\x83n\4\0\0\0\0\x80", encode(2147483648))
        self.assertEqual("\x83n\4\1\1\0\0\x80", encode(-2147483649))
        self.assertEqual("\x83o\0\0\1\0\0" + "\0" * 255 + "\1",
            encode(2 ** 2040))
        self.assertEqual("\x83o\0\0\1\0\1" + "\0" * 255 + "\1",
            encode(-2 ** 2040))

def get_suite():
    load = unittest.TestLoader().loadTestsFromTestCase
    suite = unittest.TestSuite()
    suite.addTests(load(AtomTestCase))
    suite.addTests(load(StringTestCase))
    suite.addTests(load(ImproperListTestCase))
    suite.addTests(load(OpaqueObjectTestCase))
    suite.addTests(load(DecodeTestCase))
    suite.addTests(load(EncodeTestCase))
    return suite
