# Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
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

from erlport import Atom
from erlport.erlang import RedirectedStdin, RedirectedStdout


class TestPort(object):

    def write(self, data):
        return data

class RedirectedStdinTestCase(unittest.TestCase):

    def test_read(self):
        stdin = RedirectedStdin()
        self.assertRaises(RuntimeError, stdin.read)

class RedirectedStdoutTestCase(unittest.TestCase):

    def test_write(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual((Atom("P"), "data"), stdout.write("data"))
        self.assertRaises(TypeError, stdout.write, 1234)

    def test_writelines(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual((Atom("P"), "data"), stdout.writelines(["da", "ta"]))
        self.assertRaises(TypeError, stdout.writelines, ["da", 1234])

    def test_unsupported_methods(self):
        stdout = RedirectedStdout(TestPort())
        self.assertRaises(RuntimeError, stdout.read)


def get_suite():
    load = unittest.TestLoader().loadTestsFromTestCase
    suite = unittest.TestSuite()
    suite.addTests(load(RedirectedStdinTestCase))
    suite.addTests(load(RedirectedStdoutTestCase))
    return suite
