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

require 'test/unit'
require 'erlport/erlproto'
require 'erlport/erlterms'

include ErlProto
include ErlTerm

class TestPortClient
    attr_reader :port

    def initialize *args
        r, @out = IO.pipe
        @in, w = IO.pipe
        @port = Port.new(4, true, false, descriptors=[r.fileno, w.fileno])
    end

    def read
        @in.sysread(65536)
    end

    def write data
        @out.syswrite(data)
    end

    def close
        @in.close
        @out.close
    end
end

class PortTestCase < Test::Unit::TestCase
    def test_default_port_read
        client = TestPortClient.new()
        assert_equal 12, client.write("\0\0\0\10\x83d\0\4test")
        atom = client.port.read()
        assert atom.is_a? Atom
        assert_equal Atom.new("test"), atom
    end

    def test_default_port_write
        client = TestPortClient.new()
        assert_equal 12, client.port.write(Atom.new("test"))
        assert_equal "\0\0\0\10\x83d\0\4test", client.read
    end
end
