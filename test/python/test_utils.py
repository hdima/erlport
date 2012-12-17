from erlport import Atom
from erlport.erlang import Modules

def switch(n):
    result = 0
    for i in range(n):
        result = Modules.python_tests.test_callback(result, i)
    return n

def identity(v):
    return v

def length(v):
    return len(v)
