from erlport import Atom, erlang


def switch(n):
    result = 0
    for i in range(n):
        result = erlang.call(Atom("python_tests"), Atom("test_callback"),
            [result, i])
    return n
