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
from optparse import OptionParser, OptionValueError
from Queue import Queue
from threading import Thread

from erlport import Port, Atom


class ReceiveThread(Thread):

    def __init__(self, port, sender, manager):
        self.port = port
        self.sender = sender
        self.manager = manager
        Thread.__init__(self)
        self.setDaemon(True)

    def run(self):
        while True:
            try:
                message = self.port.read()
            except EOFError:
                break
            self.handle(message)

    def handle(self, message):
        if isinstance(message, tuple) and len(message) >= 3:
            mtype = message[0]
            if isinstance(mtype, Atom):
                if mtype == "S":
                    if len(message) == 5:
                        id, module, function, args = message[1:]
                        result = self.call(module, function, args)
                        self.sender.send((Atom("R"), id, result))
                        return
                elif mtype == "A":
                    if len(message) == 4:
                        module, function, args = message[1:]
                        self.call(module, function, args)
                        return
                elif mtype == "R":
                    if len(message) == 3:
                        id, result = message[1:]
                        queue = self.manager.requests[id]
                        if queue is not None:
                            queue.put(result)
                        return

    def call(self, module, function, args):
        m = sys.modules.get(module)
        if m is None:
            m = __import__(module, {}, {}, [function])
        f = getattr(m, function)
        return f(*args)


class SendThread(Thread):

    def __init__(self, port):
        self.port = port
        self.requests = Queue()
        Thread.__init__(self)
        self.setDaemon(True)

    def run(self):
        while True:
            message = self.requests.get()
            try:
                self.port.write(message)
            except EOFError:
                break

    def send(self, message):
        self.requests.put(message)


class ThreadedCallProtocol(object):

    def __init__(self, port):
        self.id = 0
        self.requests = {}
        self.port = port
        self.sender = SendThread(self.port)
        self.sender.start()
        self.receiver = ReceiveThread(self.port, self.sender, self)
        self.receiver.start()

    def cast(self, module, function, args):
        request = Atom("A"), Atom(module), Atom(function), list(args)
        self.sender.send(request)

    def call(self, module, function, args):
        id = self.id
        # TODO: Optimize id generation
        # TODO: Cleanup requests storage
        self.id += 1
        request = Atom("S"), id, Atom(module), Atom(function), list(args)
        queue = Queue()
        self.requests[id] = queue
        self.sender.send(request)
        response = queue.get()
        del self.requests[id]
        return response


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


def get_option_parser():
    def packet_option_handler(option, opt_str, value, parser):
        if value not in (1, 2, 4):
            raise OptionValueError("Valid values for --packet are 1, 2, or 4")
        setattr(parser.values, option.dest, value)
    parser = OptionParser()
    parser.add_option("--packet", action="callback", type="int",
        help="Message length sent in N bytes. Valid values are 1, 2, or 4",
        metavar="N", callback=packet_option_handler, default=4)
    parser.add_option("--nouse_stdio", action="store_false",
        dest="stdio", default=True,
        help="Use file descriptors 3 and 4 for communication with Erlang")
    parser.add_option("--use_stdio", action="store_true", dest="stdio",
        default=True,
        help="Use file descriptors 0 and 1 for communication with Erlang")
    return parser


def main(args=None):
    parser = get_option_parser()
    options, args = parser.parse_args(args)
    port = Port(use_stdio=options.stdio, packet=options.packet)
    proto = ThreadedCallProtocol(port)

    import time

    while True:
        r = proto.call(Atom("calendar"), Atom("universal_time"), [])
        print >>sys.stderr, "Result: %r" % (r,)
        time.sleep(2)


if __name__ == "__main__":
    main()
