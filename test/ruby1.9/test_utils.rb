include ErlPort
include ErlPort::ErlTerm

def switch n
    result = 0
    for i in 0...n
        result = Erlang.call :ruby19_tests, :test_callback, [result, i]
    end
    n
end

def identity v
    v
end

def add a, b
    a + b
end

def len s
    s.length
end

def print_string s
    puts s.pack "U*"
end

def string_to_sym s
    s.pack("U*").to_sym
end

module TestModule
    class TestClass
        def self.test_method
            :ok
        end
    end
end

def setup_date_types
    Erlang.set_encoder {|v| date_encoder v}
    Erlang.set_decoder {|v| date_decoder v}
    :ok
end

def date_encoder value
    if value.is_a? Time
        value = Tuple.new([:date,
            Tuple.new([value.year, value.month, value.day])])
    end
    value
end

def date_decoder value
    if value.is_a? Tuple and value.length == 2 and value[0] == :date
        year, month, day = value[1]
        value = Time.utc(year, month, day)
    end
    value
end
