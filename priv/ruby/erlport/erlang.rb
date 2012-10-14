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

require "erlport/erlterms"

include ErlTerm

class ErlPortError < Exception
end

class InvalidMode < ErlPortError
end

class InvalidMessage < ErlPortError
end

class UnknownMessage < ErlPortError
end

module Erlang
    module_function
    def start port
        @@port = port
        @@client = false
        begin
            self.loop
        rescue EOFError
        end
    end

    module_function
    def call mod, function, args
        raise InvalidMode, "you need call Erlang.start(port) function first" \
            if @@port == nil
        # TODO
    end

    private

    module_function
    def loop
        switch_ack = Atom.new("s")
        while true
            message = @@port.read
            raise InvalidMessage, "invalid message: #{message}" \
                if message.length != 4
            mtype, mod, function, args = message
            case mtype
                when "C"
                    @@port.write(
                        self.call_with_error_handler(mod, function, args))
                when "S"
                    @@port.write(switch_ack)
                    @@client = true
                    @@port.write(
                        self.call_with_error_handler(mod, function, args))
                    @@client = false
                default
                    raise UnknownMessage, "unknown message: #{message}"
            end
        end
    end

    module_function
    def call_with_error_handler mod, function, args
        begin
            require mod if mod != ""
            idx = function.rindex("::")
            if idx == nil
                # TODO: Forbid calls without a library?
                r = send(function, *args)
            else
                container = eval function[0...idx]
                fun = function[idx + 2..-1]
                r = container.send(fun, *args)
            end
            # TODO: Encode result
            Tuple.new([Atom.new("r"), r])
        rescue Exception => why
            # TODO: Update exception format
            exc = Atom.new(why.inspect())
            exc_tb = why.backtrace().reverse()
            Tuple.new([Atom.new("e"), [exc, why.message, exc_tb]])
        end
    end
end
