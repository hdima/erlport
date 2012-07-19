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

import sys
from optparse import OptionParser, OptionValueError

from erlport import erlang
from erlport import Port


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
    parser.add_option("--client", dest="client",
        help="Set 'client mode' module for Python", metavar="FILE")
    return parser


def main(args=None):
    parser = get_option_parser()
    options, args = parser.parse_args(args)
    port = Port(use_stdio=options.stdio, packet=options.packet)
    erlang.start(port, options.client)


if __name__ == "__main__":
    main()
