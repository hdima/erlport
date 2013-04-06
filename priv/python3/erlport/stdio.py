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

import sys

from erlport import Atom


class RedirectedStdin(object):

    def __getattr__(self, name):
        def closed(*args, **kwargs):
            raise RuntimeError("STDIN is closed for ErlPort connected process")
        return closed

class RedirectedStdout(object):

    def __init__(self, port):
        self.__port = port

    def write(self, data):
        if not isinstance(data, str):
            raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.__port.write((Atom(b"P"), data))

    def writelines(self, lst):
        for data in lst:
            if not isinstance(data, str):
                raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.write("".join(lst))

    def __getattr__(self, name):
        def unsupported(*args, **kwargs):
            raise RuntimeError("unsupported STDOUT operation for ErlPort"
                " connected process")
        return unsupported


def redirect(port):
    sys.stdin = RedirectedStdin()
    sys.stdout = RedirectedStdout(port)
