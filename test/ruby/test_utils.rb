include Erlang

def switch n
    result = 0
    for i in 0...n
        result = Modules.ruby_tests.test_callback result, i
    end
    n
end

module Test
    module_function
    def add a, b
        a + b
    end

    module_function
    def len s
        s.length
    end
end
