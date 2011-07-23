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

import sys

from erlport import Port, Atom


class CallProtocol(object):

    def start(self, port):
        while True:
            try:
                message = port.read()
            except EOFError:
                break
            self.handle(port, message)

    def handle(self, port, message):
        if isinstance(message, tuple) and len(message) >= 4:
            mtype = message[0]
            if isinstance(mtype, Atom):
                if mtype == "S":
                    if len(message) == 5:
                        id, module, function, args = message[1:]
                        result = self.call(module, function, args)
                        port.write((Atom("R"), id, result))
                        return
                elif mtype == "A":
                    if len(message) == 4:
                        module, function, args = message[1:]
                        self.call(module, function, args)
                        return
        port.write((Atom("error"), Atom("badarg")))

    def call(self, module, function, args):
        m = sys.modules.get(module)
        if m is None:
            m = __import__(module, {}, {}, [function])
        f = getattr(m, function)
        return f(*args)


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    proto = CallProtocol()
    proto.start(Port(use_stdio=True, packet=4))


if __name__ == "__main__":
    main()
