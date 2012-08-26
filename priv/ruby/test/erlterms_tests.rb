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
require 'erlport/erlterms'

include ErlTerm


class DecodeTest < Test::Unit::TestCase
    def test_decode
        assert_raise(IncompleteData){decode("")}
        assert_raise(ValueError){decode("\0")}
        assert_raise(IncompleteData){decode("\x83")}
        assert_raise(ValueError){decode("\x83z")}
    end

    def test_decode_atom
        assert_raise(IncompleteData){decode("\x83d")}
        assert_raise(IncompleteData){decode("\x83d\0")}
        assert_raise(IncompleteData){decode("\x83d\0\1")}
        assert_equal Atom, decode("\x83d\0\0")[0].class
        assert_equal [Atom.new(""), ""], decode("\x83d\0\0")
        assert_equal [Atom.new(""), "tail"], decode("\x83d\0\0tail")
        assert_equal [Atom.new("test"), ""], decode("\x83d\0\4test")
        assert_equal [Atom.new("test"), "tail"], decode("\x83d\0\4testtail")
    end

    def test_decode_predefined_atoms
        assert_equal [true, ""], decode("\x83d\0\4true")
        assert_equal [true, "tail"], decode("\x83d\0\4truetail")
        assert_equal [false, ""], decode("\x83d\0\5false")
        assert_equal [false, "tail"], decode("\x83d\0\5falsetail")
        assert_equal [nil, ""], decode("\x83d\0\11undefined")
        assert_equal [nil, "tail"], decode("\x83d\0\11undefinedtail")
    end

    def test_decode_empty_list
        assert_equal [[], ""], decode("\x83j")
        assert_equal [[], "tail"], decode("\x83jtail")
    end

    def test_decode_string_list
        assert_raise(IncompleteData){decode("\x83k")}
        assert_raise(IncompleteData){decode("\x83k\0")}
        assert_raise(IncompleteData){decode("\x83k\0\1")}
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], decode("\x83k\0\0")
        assert_equal [[], "tail"], decode("\x83k\0\0tail")
        assert_equal [[116, 101, 115, 116], ""], decode("\x83k\0\4test")
        assert_equal [[116, 101, 115, 116], "tail"],
            decode("\x83k\0\4testtail")
    end

    def test_decode_list
        assert_raise(IncompleteData){decode("\x83l")}
        assert_raise(IncompleteData){decode("\x83l\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0\0\0")}
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], decode("\x83l\0\0\0\0j")
        assert_equal [[], "tail"], decode("\x83l\0\0\0\0jtail")
        assert_equal [[[], []], ""], decode("\x83l\0\0\0\2jjj")
        assert_equal [[[], []], "tail"], decode("\x83l\0\0\0\2jjjtail")
    end

    def test_decode_small_tuple
        assert_raise(IncompleteData){decode("\x83h")}
        assert_raise(IncompleteData){decode("\x83h\1")}
        assert_equal Tuple, decode("\x83h\0")[0].class
        assert_equal [Tuple.new([]), ""], decode("\x83h\0")
        assert_equal [Tuple.new([]), "tail"], decode("\x83h\0tail")
        assert_equal [Tuple.new([[], []]), ""], decode("\x83h\2jj")
        assert_equal [Tuple.new([[], []]), "tail"], decode("\x83h\2jjtail")
    end
end
