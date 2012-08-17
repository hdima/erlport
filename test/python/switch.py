from erlport import Atom, Erlang


def switch(n):
    result = 0
    for i in range(n):
        result = Erlang.python_tests.test_callback(result, i)
    return n
