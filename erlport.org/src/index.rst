Installation
------------

Easiest method to install **ErlPort** is to use ``easy_install`` tool from
`setuptools <http://pypi.python.org/pypi/setuptools>`_ package::

    $ easy_install erlport

Main prerequisites:

- `Erlang <http://erlang.org>`_ >= R11B-4
- `Python <http://python.org>`_ >= 2.4

Also source code of the library can be obtained from `GitHub
<http://github.com/hdima/erlport>`_ or `PyPi
<http://pypi.python.org/pypi/erlport>`_.

About
-----

**ErlPort** is a `Python <http://python.org>`_ library which implements `Erlang
external term format <http://www.erlang.org/doc/apps/erts/erl_ext_dist.html>`_
and `Erlang port protocol <http://erlang.org/doc/man/erlang.html#open_port-2>`_
for easier integration of `Erlang <http://erlang.org>`_ and `Python
<http://python.org>`_.

Check out the following topics:

- `Internals <internals.html>`_ - How Erlang and Python integration works
- `Recipes <recipes.html>`_ - **ErlPort** recipes
- `Contributors <contributors.html>`_ - **ErlPort** contributors

Feedback
--------

Please report bugs, offer suggestions or feedback at:

- Report bugs at `GitHub issue tracker
  <http://github.com/hdima/erlport/issues>`_

- `Email me <mailto:dima%20at%20hlabs.org>`_

- Write or follow me at `@hdima <http://twitter.com/hdima>`_

Example
-------

Erlang module (hello.erl):

.. sourcecode:: erlang

    -module(hello).
    -export([hello/1]).


    hello(Name) ->
        % Spawn hello.py script and open communication channels
        Port = open_port({spawn, "python -u hello.py"},
            [{packet, 1}, binary, use_stdio]),
        % Convert tuple {hello, Name} to external term format
        ReqData = term_to_binary({hello, Name}),
        % Send binary data to hello.py script
        port_command(Port, ReqData),
        % Wait for reply from hello.py script
        receive
            {Port, {data, RespData}} ->
                % Convert binary data to term
                {ok, binary_to_term(RespData)}
        after
            5000 ->
                {error, timeout}
        end.

Python module (hello.py):

.. sourcecode:: python

    from erlport import Port, Protocol, String


    # Inherit custom protocol from erlport.Protocol
    class HelloProtocol(Protocol):

        # Function handle_NAME will be called for incoming tuple {NAME, ...}
        def handle_hello(self, name):
            # String wrapper forces name to be a string instead of a list
            return "Hello, %s" % String(name)


    if __name__ == "__main__":
        proto = HelloProtocol()
        # Run protocol with port open on STDIO
        proto.run(Port(use_stdio=True))

Test the modules above in the Erlang shell:

.. sourcecode:: erlang

    1> % Compile hello.erl module
    1> c(hello).
    {ok,hello}
    2> % Call hello:hello() -> HelloProtocol.handle_hello()
    2> hello:hello("Bob").
    {ok,"Hello, Bob"}

.. |date| date::
.. container:: date

    Updated on |date|
