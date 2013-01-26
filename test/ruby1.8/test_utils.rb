include ErlPort

def switch n
    result = 0
    for i in 0...n
        result = Erlang.call :ruby18_tests, :test_callback, [result, i]
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

module TestModule
    class TestClass
        def self.test_method
            :ok
        end
    end
end
