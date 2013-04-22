.. class:: sidebar

Downloads
---------

- Download `ErlPort 1.0.0alpha <downloads/erlport-1.0.0alpha.zip>`__.

- Download old `ErlPort 0.7 <downloads/erlport-0.7.zip>`__.

ErlPort source code can be obtained from `GitHub
<http://github.com/hdima/erlport>`__.

About
-----

ErlPort is a library for `Erlang <http://erlang.org>`__ which can easily
connect Erlang and a number of other programming languages. The library use
`Erlang external term format
<http://erlang.org/doc/apps/erts/erl_ext_dist.html>`__ and `Erlang port
protocol <http://erlang.org/doc/man/erlang.html#open_port-2>`__ to simplify
connection between the languages.

Example usage in Erlang shell:

.. sourcecode:: erl

    1> {ok, P} = python:start().
    {ok, <0.34.0>}
    2> python:call(P, operator, add, [2, 2]).
    4
    3> python:call(P, '__builtin__', len, [processes()]).
    26
    4> python:stop(P)
    ok

Check more examples at the `Examples <examples/>`__ page.

Supported language versions:

+---------------------------------+--------+
| `Erlang <http://erlang.org>`__  | >= R13 |
+---------------------------------+--------+
| `Python <python.html>`__        | >= 2.5 |
+---------------------------------+--------+
| `Ruby <ruby.html>`__            | >= 1.8 |
+---------------------------------+--------+

Features
--------

Use cases
---------

.. class:: sidebar

Feedback
--------

Please report bugs, offer suggestions or feedback at:

- Report bugs at `GitHub issue tracker
  <http://github.com/hdima/erlport/issues>`__

- `Email me <mailto:dima%20at%20hlabs.org>`__

- Write or follow me at `@hdima <http://twitter.com/hdima>`__

.. |date| date::
.. container:: date

    Updated on |date|
