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

class Atom < String
    def initialize data
        raise ValueError, "invalid atom length" if data.length > 255
        super data
    end
end

class Tuple < Array
end

module ErlTerm
    class IncompleteData < Exception
        def initialize string
            super "incomplete data: '#{string}'"
        end
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
                    # TODO: Improper lists
                    raise ValueError, "improper list" if tail[0] != 106
                    return lst, tail[1..-1]
                end
                # TODO: Opaque objects
                return Tuple.new(lst), tail
            when 97
                # SMALL_INTEGER_EXT
                raise IncompleteData, string if string.length < 2
                return string[1], string[2..-1]
            when 98
                # INTEGER_EXT
                raise IncompleteData, string if string.length < 5
                return string[1,4].unpack("N")[0], string[5..-1]
        end

        raise ValueError, "unsupported data: '%s'" % string
    end
end
