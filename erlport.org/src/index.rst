.. class:: sidebar

Downloads
---------

Also source code of the library can be obtained from `GitHub
<http://github.com/hdima/erlport>`__

About
-----

ErlPort is a library for Erlang which can easily connect Erlang and a number of
other programming languages. The library use `Erlang external term format
<http://erlang.org/doc/apps/erts/erl_ext_dist.html>`__ and `Erlang port
protocol <http://erlang.org/doc/man/erlang.html#open_port-2>`__ to simplify
connection between programming languages.

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

Example
-------

.. sourcecode:: erl

    1> {ok, P} = python:start().
    {ok, <0.34.0>}
    2> {ok, R} = ruby:start().
    {ok, <0.35.0>}
    3> python:call(P, os, getpid, []).
    8878
    4> ruby:call(R, '', 'Process::pid', []).
    8882
    5> python:call(P, 'erlport.erlang', call,
    5>             [ruby, call, [R, '', 'Process::pid', []]]).
    8882

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
