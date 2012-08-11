from erlport import Atom, erlang


def switch(n):
    result = 0
    for i in range(n):
        result = erlang.modules.python_tests.test_callback(result, i)
    return n
