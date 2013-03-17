from erlport import Atom, erlang

def switch(n):
    result = 0
    for i in range(n):
        _, result = erlang.call(Atom("python2_tests"), Atom("test_callback"),
            [(result, i)])
    return n

def setup_message_handler():
    def handler(message):
        erlang.call(Atom("python2_tests"), Atom("test_callback"),
            [(Atom("message"), message)])
    erlang.set_message_handler(handler)

def identity(v):
    return v

def length(v):
    return len(v)

def print_string(s):
    print s.to_string()

class TestClass(object):
    class TestSubClass(object):
        @staticmethod
        def test_method():
            return Atom("ok")
