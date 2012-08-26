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

#
# Erlang external term format.
#
# See Erlang External Term Format for details:
#    http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
#

class ValueError < Exception
end

class IncompleteData < Exception
    def initialize string
        super "incomplete data: '#{string}'"
    end
end

class Atom < String
    def initialize data
        raise ValueError, "invalid atom length" if data.length > 255
        super data
    end
end

class Tuple < Array
end

# TODO: Add some method to String to convert from arrays?

class ImproperList < Array
    attr_accessor :tail

    def initialize array, tail
        raise ValueError, "empty list not allowed" if array.empty?
        raise TypeError, "non list object expected for tail" \
            if tail.is_a? Array
        @tail = tail
        super array
    end
end

module ErlTerm
    class OpaqueObject
        attr_accessor :data
        attr_accessor :language

        MARKER = Atom.new("$erlport.opaque")

        def initialize data, language
            raise TypeError, "data must be instance of String" \
                if not data.is_a? String
            raise TypeError, "language must be instance of Atom" \
                if not language.is_a? Atom
            @data = data
            @language = language
        end

        def self.decode data, language
            return Marshal.load(data) if language == "ruby"
            return OpaqueObject.new data, language
        end

        def encode
            return @data if @language == "erlang"
            return encode_term(Tuple.new([MARKER, @language, @data]))
        end

        # TODO: Add 'hash' and 'eq' methods
        # TODO: Tests for OpaqueObject
    end

    module_function
    def decode string
        raise IncompleteData, string if string == ""
        raise ValueError, "unknown protocol version: %s" % string[0] \
            if string[0] != 131
        # TODO: Compressed terms
        decode_term string[1..-1]
    end

    private

    module_function
    def decode_term string
        raise IncompleteData, string if string == ""
        tag = string[0]
        case tag
            when 100
                # ATOM_EXT
                ln = string.length
                raise IncompleteData, string if ln < 3
                length = string[1,2].unpack("n")[0] + 3
                raise IncompleteData, string if ln < length
                name = string[3...length]
                if name == "true":
                    return true, string[length..-1]
                elsif name == "false":
                    return false, string[length..-1]
                elsif name == "undefined":
                    return nil, string[length..-1]
                end
                return Atom.new(name), string[length..-1]
            when 106
                # NIL_EXT
                return [], string[1..-1]
            when 107
                # STRING_EXT
                ln = string.length
                raise IncompleteData, string if ln < 3
                length = string[1,2].unpack("n")[0] + 3
                raise IncompleteData, string if ln < length
                return string[3...length].unpack("C*"), string[length..-1]
            when 108, 104, 105
                if tag == 104
                    raise IncompleteData, string if string.length < 2
                    length = string[1]
                    tail = string[2..-1]
                else
                    raise IncompleteData, string if string.length < 5
                    length = string[1,4].unpack("N")[0]
                    tail = string[5..-1]
                end
                lst = []
                while length > 0
                    term, tail = decode_term(tail)
                    lst.push(term)
                    length -= 1
                end
                if tag == 108
                    raise IncompleteData, string if tail == ""
                    if tail[0] != 106
                        improper_tail, tail = decode_term(tail)
                        return ImproperList.new(lst, improper_tail), tail
                    end
                    return lst, tail[1..-1]
                end
                return [OpaqueObject.decode(lst[2], lst[1]), tail] \
                    if lst.length == 3 and lst[0] == OpaqueObject::MARKER
                return Tuple.new(lst), tail
            when 97
                # SMALL_INTEGER_EXT
                raise IncompleteData, string if string.length < 2
                return string[1], string[2..-1]
            when 98
                # INTEGER_EXT
                raise IncompleteData, string if string.length < 5
                int = string[1,4].unpack("N")[0]
                int -= 0x100000000 if int > 0x7fffffff
                return int, string[5..-1]
            when 109
                # BINARY_EXT
                ln = string.length
                raise IncompleteData, string if ln < 5
                length = string[1,4].unpack("N")[0] + 5
                raise IncompleteData, string if ln < length
                return string[5...length], string[length..-1]
            when 70
                # NEW_FLOAT_EXT
                raise IncompleteData, string if string.length < 9
                return string[1,8].unpack("G")[0], string[9..-1]
            when 110, 111
                # SMALL_BIG_EXT, LARGE_BIG_EXT
                if tag == 110
                    raise IncompleteData, string if string.length < 3
                    length, sign = string[1,2].unpack("CC")
                    tail = string[3..-1]
                else
                    raise IncompleteData, string if string.length < 6
                    length, sign = string[1,5].unpack("NC")
                    tail = string[6..-1]
                end
                raise IncompleteData, string if tail.length < length
                n = 0
                if length > 0
                    for i in tail[0,length].unpack("C*").reverse!
                        n = (n << 8) | i
                    end
                    n = -n if sign != 0
                end
                return n, tail[length..-1]
        end

        raise ValueError, "unsupported data: '%s'" % string
    end
end
