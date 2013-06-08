from datetime import date, timedelta

from erlport import Atom, erlang

def switch(n):
    result = 0
    for i in range(n):
        _, result = erlang.call(Atom(b"python3_tests"), Atom(b"test_callback"),
            [(result, i)])
    return n

def setup_message_handler():
    def handler(message):
        erlang.call(Atom(b"python3_tests"), Atom(b"test_callback"),
            [(Atom(b"message"), message)])
    erlang.set_message_handler(handler)
    return Atom(b"ok")

def setup_faulty_message_handler():
    def handler(message):
        raise ValueError(message)
    erlang.set_message_handler(handler)
    return Atom(b"ok")

def recurse(python, n):
    if n <= 0:
        return Atom(b"done")
    return erlang.call(Atom(b"python3_tests"), Atom(b"recurse"),
        [python, n - 1])

def identity(v):
    return v

def length(v):
    return len(v)

def print_string(s):
    print(s.to_string())

class TestClass(object):
    class TestSubClass(object):
        @staticmethod
        def test_method():
            return Atom(b"ok")

def setup_date_types():
    erlang.set_encoder(date_encoder)
    erlang.set_decoder(date_decoder)
    return Atom(b"ok")

def date_encoder(value):
    if isinstance(value, date):
        value = Atom(b"date"), (value.year, value.month, value.day)
    elif isinstance(value, timedelta):
        value = Atom(b"days"), value.days
    return value

def date_decoder(value):
    if isinstance(value, tuple) and len(value) == 2:
        if value[0] == b"date":
            year, month, day = value[1]
            value = date(year, month, day)
        elif value[0] == b"days":
            value = timedelta(days=value[1])
    return value
