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


class DecodeTest < Test::Unit::TestCase
    def test_decode
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("")
        end
        assert_raise ValueError do
            ErlTerm.decode("\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83")
        end
        assert_raise ValueError do
            ErlTerm.decode("\x83z")
        end
    end

    def test_decode_atom
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83d")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83d\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83d\0\1")
        end
        atom, _ = ErlTerm.decode("\x83d\0\0")
        assert_equal Atom, atom.class
        assert_equal [Atom.new(""), ""], ErlTerm.decode("\x83d\0\0")
        assert_equal [Atom.new(""), "tail"], ErlTerm.decode("\x83d\0\0tail")
        assert_equal [Atom.new("test"), ""], ErlTerm.decode("\x83d\0\4test")
        assert_equal [Atom.new("test"), "tail"],
            ErlTerm.decode("\x83d\0\4testtail")
    end

    def test_decode_predefined_atoms
        assert_equal [true, ""], ErlTerm.decode("\x83d\0\4true")
        assert_equal [true, "tail"], ErlTerm.decode("\x83d\0\4truetail")
        assert_equal [false, ""], ErlTerm.decode("\x83d\0\5false")
        assert_equal [false, "tail"], ErlTerm.decode("\x83d\0\5falsetail")
        assert_equal [nil, ""], ErlTerm.decode("\x83d\0\11undefined")
        assert_equal [nil, "tail"], ErlTerm.decode("\x83d\0\11undefinedtail")
    end

    def test_decode_empty_list
        assert_equal [[], ""], ErlTerm.decode("\x83j")
        assert_equal [[], "tail"], ErlTerm.decode("\x83jtail")
    end

    def test_decode_string_list
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83k")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83k\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83k\0\1")
        end
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], ErlTerm.decode("\x83k\0\0")
        assert_equal [[], "tail"], ErlTerm.decode("\x83k\0\0tail")
        assert_equal [[116, 101, 115, 116], ""],
            ErlTerm.decode("\x83k\0\4test")
        assert_equal [[116, 101, 115, 116], "tail"],
            ErlTerm.decode("\x83k\0\4testtail")
    end

    def test_decode_list
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83l")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83l\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83l\0\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83l\0\0\0")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83l\0\0\0\0")
        end
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], ErlTerm.decode("\x83l\0\0\0\0j")
        assert_equal [[], "tail"], ErlTerm.decode("\x83l\0\0\0\0jtail")
        assert_equal [[[], []], ""], ErlTerm.decode("\x83l\0\0\0\2jjj")
        assert_equal [[[], []], "tail"], ErlTerm.decode("\x83l\0\0\0\2jjjtail")
    end

    def test_decode_small_tuple
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83h")
        end
        assert_raise ErlTerm::IncompleteData do
            ErlTerm.decode("\x83h\1")
        end
        tuple, _ = ErlTerm.decode("\x83h\0")
        assert_equal Tuple, tuple.class
        assert_equal [Tuple.new([]), ""], ErlTerm.decode("\x83h\0")
        assert_equal [Tuple.new([]), "tail"], ErlTerm.decode("\x83h\0tail")
        assert_equal [Tuple.new([[], []]), ""], ErlTerm.decode("\x83h\2jj")
        assert_equal [Tuple.new([[], []]), "tail"],
            ErlTerm.decode("\x83h\2jjtail")
    end
end
