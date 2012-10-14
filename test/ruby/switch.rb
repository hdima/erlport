def switch n
    result = 0
    for i in 0...n
        result = Erlang::call(:ruby_tests, :test_callback, [result, i])
    end
    n
end
