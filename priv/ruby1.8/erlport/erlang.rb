# Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
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

require "thread"

require "erlport/erlterms"
require "erlport/stdio"

include ErlPort::ErlTerm

module ErlPort
module Erlang

    class ErlPortError < Exception
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
            super value.to_s
        end
    end

    module_function
    def cast pid, message
        # It's safe to call it from multiple threads because port.write will be
        # locked
        @@port.write(Tuple.new([:M, pid, message]))
        nil
    end

    module_function
    def call mod, function, args
        raise ValueError, mod \
            if not (mod.is_a? Symbol or mod.is_a? EmptySymbol)
        raise ValueError, function \
            if not (function.is_a? Symbol or function.is_a? EmptySymbol)
        raise ValueError, args if not args.is_a? Array
        _call mod, function, args, :N
    end

    module_function
    def self
        @@self = _call(:erlang, :self, [], :L) if @@self == nil
        @@self
    end

    module_function
    def make_ref
        _call(:erlang, :make_ref, [], :L)
    end

    module_function
    def set_default_encoder
        @@encoder = lambda {|v| v}
    end

    module_function
    def set_encoder &encoder
        check_handler encoder
        @@encoder = encoder
    end

    module_function
    def set_default_decoder
        @@decoder = lambda {|v| v}
    end

    module_function
    def set_decoder &decoder
        check_handler decoder
        @@decoder = decoder
    end

    module_function
    def start port
        setup port
        # Remove ErlPort::Erlang::start function
        Erlang.instance_eval {undef :start}
        begin
            self.loop
        rescue EOFError
        end
    end

    private

    module_function
    def setup port
        @@port = port
        @@self = nil
        @@call_lock = Mutex.new
        ErlPort::StdIO::redirect port
        set_default_encoder
        set_default_decoder
    end

    module_function
    def check_handler handler
        raise ValueError, "expected single argument block: #{handler}" \
            if handler.arity != 1
    end

    module_function
    def _call mod, function, args, context
        response = @@call_lock.synchronize {
            # TODO: Message ID hardcoded to 1 for now
            @@port.write(Tuple.new([:C, 1, mod, function,
                args.map(&@@encoder), context]))
            @@port.read
        }
        raise InvalidMessage, response if not response.is_a? Tuple \
            or response.length != 3
        mtype, _mid, value = response

        if mtype != :r
            raise CallError, value if mtype == :e
            raise UnknownMessage, response
        end
        @@decoder.call(value)
    end

    module_function
    def loop
        while true
            message = @@port.read
            raise InvalidMessage, message \
                if not message.is_a? Tuple or message.length != 5
            mtype, mid, mod, function, args = message

            raise UnknownMessage, message if mtype != :C

            @@port.write(self.call_with_error_handler(mid, mod, function, args))
        end
    end

    module_function
    def call_with_error_handler mid, mod, function, args
        begin
            m = mod.to_s
            args = args.map(&@@decoder)
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
            Tuple.new([:r, mid, @@encoder.call(r)])
        rescue Exception => why
            exc = why.class.to_s.to_sym
            exc_tb = why.backtrace.reverse
            value = Tuple.new([:ruby, exc, why.message, exc_tb])
            Tuple.new([:e, mid, value])
        end
    end
end
end
