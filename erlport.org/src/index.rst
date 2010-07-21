ErlPort - Erlang port protocol for Python
=========================================

About
-----

The **erlport** is a `Python <http://python.org>`_ library which implements
`Erlang external term format
<http://www.erlang.org/doc/apps/erts/erl_ext_dist.html>`_ and `Erlang port
protocol <http://erlang.org/doc/man/erlang.html#open_port-2>`_ for easier
integration of `Erlang <http://erlang.org>`_ and `Python <http://python.org>`_.

Download
--------

* `ErlPort at GitHub <http://github.com/hdima/erlport>`_
* `ErlPort at PyPi <http://pypi.python.org/pypi/erlport>`_

Example
-------

.. container::

    Python code (hello.py)::

        from erlport import Port, Protocol, String

        class HelloProtocol(Protocol):

            def handle_hello(self, name):
                return "Hello, %s" % String(name)

        if __name__ == "__main__":
            proto = HelloProtocol()
            proto.run(Port(use_stdio=True))

.. container::

    Erlang code (hello.erl)::

        -module(hello).
        -export([hello/1]).

        hello(Name) ->
            Port = open_port({spawn, "python -u hello.py"},
                [{packet, 1}, binary]),
            port_command(Port, term_to_binary({hello, Name})),
            receive
                {Port, {data, Data}} ->
                    binary_to_term(Data)
            end.

.. container::

    Test in Erlang shell::

        1> c(hello).
        {ok,hello}
        2> hello:hello("Bob").
        "Hello, Bob"
