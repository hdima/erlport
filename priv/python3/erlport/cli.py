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

from argparse import ArgumentParser, Action

from erlport import erlang
from erlport.erlproto import Port


class CompressLevel(Action):

    def __call__(self, parser, namespace, values, options_string=None):
        if values < 0 or values > 9:
            parser.error("Valid values for --compressed are 0..9")
        else:
            setattr(namespace, self.dest, values)

class PacketOption(Action):

    def __call__(self, parser, namespace, values, option_string=None):
        if values not in (1, 2, 4):
            parser.error("Valid values for --packet are 1, 2, or 4")
        else:
            setattr(namespace, self.dest, values)


def get_option_parser():
    parser = ArgumentParser(description="ErlPort - Erlang port protocol")
    parser.add_argument("--packet", action=PacketOption, type=int,
        help="Message length sent in N bytes. Valid values are 1, 2, or 4",
        metavar="N", default=4)
    parser.add_argument("--nouse_stdio", action="store_false",
        dest="stdio", default=True,
        help="Use file descriptors 3 and 4 for communication with Erlang")
    parser.add_argument("--use_stdio", action="store_true", dest="stdio",
        default=True,
        help="Use file descriptors 0 and 1 for communication with Erlang")
    parser.add_argument("--compressed", action=CompressLevel, type=int,
        default=0, help="Compression level", metavar="LEVEL")
    return parser


def main(argv=None):
    parser = get_option_parser()
    args = parser.parse_args(argv)
    port = Port(use_stdio=args.stdio, packet=args.packet,
        compressed=args.compressed)
    erlang.setup(port)


if __name__ == "__main__":
    main()
