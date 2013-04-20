.. class:: sidebar

Downloads
---------

Prerequisites:

- `Erlang <http://erlang.org>`__ >= R13

And also one of the following:

- `Python <http://python.org>`__ >= 2.5
- `Ruby <http://ruby-lang.org>`__ >= 1.8

Also source code of the library can be obtained from `GitHub
<http://github.com/hdima/erlport>`__

About
-----

ErlPort is a library for Erlang which can easily connect Erlang and a number of
other programming languages (currently supported: `Python <python.html>`__ and
`Ruby <ruby.html>`__). The library use `Erlang external term format
<http://erlang.org/doc/apps/erts/erl_ext_dist.html>`__ and `Erlang port
protocol <http://erlang.org/doc/man/erlang.html#open_port-2>`__ to simplify
connection between Erlang and external programming languages.

Check the corresponding language pages for details:

- `Python <python.html>`__ - Erlang and Python connection
- `Ruby <ruby.html>`__ - Erlang and Ruby connection

Example
-------

.. sourcecode:: erlang

    1> {ok, P} = python:start().
    {ok, <0.34.0>}
    2> {ok, R} = ruby:start().
    {ok, <0.35.0>}
    3> python:call(P, os, getpid, [])
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
