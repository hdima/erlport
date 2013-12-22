ErlPort - connect Erlang to other languages
===========================================

.. contents::

ErlPort is a library for `Erlang <http://erlang.org>`__ which helps connect
Erlang to a number of other programming languages. Currently supported external
languages are `Python <http://erlport.org/docs/python.html>`__ and `Ruby
<http://erlport.org/docs/ruby.html>`__. The library uses `Erlang port protocol
<http://www.erlang.org/doc/reference_manual/ports.html>`__ to simplify
connection between languages and `Erlang external term format
<http://erlang.org/doc/apps/erts/erl_ext_dist.html>`__ to set the common data
types mapping.

The following is an example ErlPort session for Python:

.. sourcecode:: erl

    1> {ok, P} = python:start().
    {ok,<0.34.0>}
    2> python:call(P, sys, 'version.__str__', []).
    <<"2.7.3 (default, Aug  1 2012, 05:14:39) \n[GCC 4.6.3]">>
    3> python:call(P, operator, add, [2, 2]).
    4
    4> python:stop(P).
    ok

Check http://erlport.org for more information.

Feedback
--------

Please use the following channels for reporting bugs, offering suggestions or
feedback:

- ErlPort issue tracker: https://github.com/hdima/erlport/issues
- Email: dima at hlabs.org
- Send a message or follow me for updates on Twitter: `@hdima
  <https://twitter.com/hdima>`_
