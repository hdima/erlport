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

class CallError < ErlPortError
    def initialize value
        if not value.is_a? Tuple or value.length != 4
            value = Tuple.new([nil, nil, value, []])
        end
        @language, @type, @value, @stacktrace = value
        super "#{value}"
    end
end

module Erlang
    module_function
    def start port
        @@port = port
        @@client = false
        $stdin = RedirectedStdin.new
        $stdout = RedirectedStdout.new port
        begin
            self.loop
        rescue EOFError
        end
    end

    module_function
    def call mod, function, args
        raise InvalidMode, "you need call Erlang.start(port) function first" \
            if @@port == nil

        raise ValueError, mod.to_s \
            if not (mod.is_a? Symbol or mod.is_a? EmptySymbol)
        raise ValueError, function.to_s \
            if not (function.is_a? Symbol or function.is_a? EmptySymbol)
        raise ValueError, args.to_s if not args.is_a? Array

        # TODO: Call lock
        @@port.write(Tuple.new([:C, mod, function, args]))
        response = @@port.read
        raise InvalidMessage, response.to_s if response.length != 2
        mtype, value = response

        if mtype != :r
            raise CallError, value if mtype == :e
            raise UnknownMessage, response.to_s
        end
        value
    end

    private

    class RedirectedStdin
        def method_missing name, *args
            raise IOError, "STDIN is closed for ErlPort connected process"
        end
    end

    class RedirectedStdout
        def initialize port
            @port = port
        end

        def write string
            @port.write(Tuple.new([:P, string]))
        end

        def method_missing name, *args
            raise IOError, "unsupported STDOUT operation for ErlPort"
                " connected process"
        end
    end

    module_function
    def loop
        switch_ack = :s
        while true
            message = @@port.read
            raise InvalidMessage, "invalid message: #{message}" \
                if message.length != 4
            mtype, mod, function, args = message
            case mtype
                when :C
                    @@port.write(
                        self.call_with_error_handler(mod, function, args))
                when :S
                    @@port.write(switch_ack)
                    @@client = true
                    @@port.write(
                        self.call_with_error_handler(mod, function, args))
                    @@client = false
                else
                    raise UnknownMessage, "unknown message: #{message}"
            end
        end
    end

    module_function
    def call_with_error_handler mod, function, args
        begin
            m = mod.to_s
            f = function.to_s
            require m if m != ""
            idx = f.rindex("::")
            if idx == nil
                r = send(f, *args)
            else
                container = eval f[0...idx]
                fun = f[idx + 2..-1]
                r = container.send(fun, *args)
            end
            # TODO: Encode result
            Tuple.new([:r, r])
        rescue Exception => why
            exc = why.class.to_s.to_sym
            exc_tb = why.backtrace.reverse
            value = Tuple.new([:ruby, exc, why.message, exc_tb])
            Tuple.new([:e, value])
        end
    end
end
