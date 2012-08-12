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
from erlport.erlterms import Atom, String, OpaqueObject, encode, decode


class AtomTestCase(unittest.TestCase):

    def test_atom(self):
        atom = Atom("test")
        self.assertEqual(Atom, type(atom))
        self.assertEqual("test", atom)
        self.assertEqual("atom('test')", repr(atom))
        self.assertTrue(atom is Atom(atom))
        self.assertEqual("X" * 255, Atom("X" * 255))
        self.assertRaises(ValueError, Atom, "X" * 256)
        self.assertRaises(TypeError, Atom, [1, 2])

class StringTestCase(unittest.TestCase):

    def test_string(self):
        string = String(u"test")
        self.assertEqual(String, type(string))
        self.assertEqual(u"test", string)
        self.assertEqual("string(u'test')", repr(string))
        self.assertTrue(string is String(string))
        self.assertEqual(u"test", String([116, 101, 115, 116]))
        self.assertRaises(TypeError, String, (1, 2))
        self.assertRaises(TypeError, String, ["a", "b"])

class OpaqueObjectTestCase(unittest.TestCase):

    def test_opaque_object(self):
        obj = OpaqueObject("data", Atom("language"))
        self.assertEqual(OpaqueObject, type(obj))
        self.assertEqual("data", obj.data)
        self.assertEqual("language", obj.language)
        self.assertEqual("opaque(atom('language'), 'data')", repr(obj))
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


def get_suite():
    load = unittest.TestLoader().loadTestsFromTestCase
    suite = unittest.TestSuite()
    suite.addTests(load(AtomTestCase))
    suite.addTests(load(StringTestCase))
    suite.addTests(load(OpaqueObjectTestCase))
    return suite
