%%% Copyright (c) 2009-2012, Dmitry Vasiliev <dima@hlabs.org>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-module(python_tests).

-export([test_callback/2]).

-include_lib("eunit/include/eunit.hrl").

-define(_assertIdentity(P, V), ?_assertEqual((V),
    python:call(P, test_utils, identity, [(V)]))).
-define(assertIdentity(P, V), ?assertEqual((V),
    python:call(P, test_utils, identity, [(V)]))).


test_callback(PrevResult, N) ->
    log_event({test_callback, PrevResult, N}),
    N.

start_stop_test_() -> [
    fun () ->
        {ok, P} = python:start(),
        ?assertEqual(ok, python:stop(P))
    end,
    fun () ->
        {ok, P} = python:start_link(),
        ?assertEqual(ok, python:stop(P))
    end,
    fun () ->
        ?assertMatch({ok, _}, python:start({local, python_test})),
        ?assertEqual(ok, python:stop(python_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, python:start_link({local, python_test})),
        ?assertEqual(ok, python:stop(python_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, python:start({local, python_test}, [])),
        ?assertEqual(ok, python:stop(python_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, python:start_link({local, python_test}, [])),
        ?assertEqual(ok, python:stop(python_test))
    end
    ].

call_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    end} || Setup <- [fun setup/0, fun setup3/0]].

call_back_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertError({python, 'erlport.erlang.InvalidMode',
                "call() is unsupported in server mode", [_|_]},
            python:call(P, 'erlport.erlang', call,
                [erlang, length, [[1, 2, 3]]])),
        ?_assertEqual(3, python:switch(P, 'erlport.erlang', call,
            [erlang, length, [[1, 2, 3]]], [wait_for_result]))
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

error_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) -> [
        ?_assertError({python, 'exceptions.ImportError',
                "No module named unknown", [_|_]},
            python:call(P, unknown, unknown, [])),
        ?_assertError({python, 'erlport.erlang.CallError',
                "(Atom('erlang'), Atom('error'), Atom('undef'), "
                "List([(Atom('unknown'), Atom('unknown'), List([]))," ++ _,
                [_|_]},
            python:switch(P, 'erlport.erlang', call,
                [unknown, unknown, []], [wait_for_result])),
        fun () ->
            P2 = setup(),
            try
                ?assertError({python, 'erlport.erlang.CallError',
                        "(Atom('python'), Atom('exceptions.ImportError'), "
                        ++ _, [_|_]},
                    python:switch(P, 'erlport.erlang', call,
                        [python, call, [P2, unknown, unknown, []]],
                        [wait_for_result]))
            after
                cleanup(P2)
            end
        end
    ] end}.

error3_test_() -> {setup,
    fun setup3/0,
    fun cleanup/1,
    fun (P) -> [
        ?_assertError({python, 'builtins.ImportError',
                "No module named unknown", [_|_]},
            python:call(P, unknown, unknown, [])),
        ?_assertError({python, 'erlport.erlang.CallError',
                "(Atom(b'erlang'), Atom(b'error'), Atom(b'undef'), "
                "List([(Atom(b'unknown'), Atom(b'unknown'), List([]))," ++ _,
                [_|_]},
            python:switch(P, 'erlport.erlang', call,
                [unknown, unknown, []], [wait_for_result])),
        fun () ->
            P2 = setup3(),
            try
                ?assertError({python, 'erlport.erlang.CallError',
                        "(Atom(b'python'), Atom(b'builtins.ImportError'), "
                        ++ _, [_|_]},
                    python:switch(P, 'erlport.erlang', call,
                        [python, call, [P2, unknown, unknown, []]],
                        [wait_for_result]))
            after
                cleanup(P2)
            end
        end
    ] end}.

stdin_stdout_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) -> [
        ?_test(erlport_test_utils:assert_output(<<"HELLO!\n">>,
            fun () -> undefined = python:call(P, '__builtin__', print,
                [<<"HELLO!">>]) end, P)),
        ?_assertError({python, 'exceptions.RuntimeError',
            "STDIN is closed for ErlPort connected process", [_|_]},
            python:call(P, '__builtin__', raw_input, []))
    ] end}.

stdin_stdout3_test_() -> {setup,
    fun setup3/0,
    fun cleanup/1,
    fun (P) -> [
        ?_test(erlport_test_utils:assert_output(<<"b'HELLO!'\n">>,
            fun () -> undefined = python:call(P, builtins, print,
                [<<"HELLO!">>]) end, P)),
        ?_assertError({python, 'builtins.RuntimeError',
            "STDIN is closed for ErlPort connected process", [_|_]},
            python:call(P, builtins, input, []))
    ] end}.

nouse_stdio_test_() ->
    case os:type() of
        {win32, _} ->
            [];
        _ ->
            [{setup,
                Setup,
                fun cleanup/1,
                fun (P) ->
                    ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
                end} || Setup <- [setup_factory([nouse_stdio]),
                    setup3_factory([nouse_stdio])]]
    end.

packet4_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    end} || Setup <- [setup_factory([{packet, 4}]),
        setup3_factory([{packet, 4}])]].

packet2_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    end} || Setup <- [setup_factory([{packet, 2}]),
        setup3_factory([{packet, 2}])]].

packet1_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    end} || Setup <- [setup_factory([{packet, 1}]),
        setup3_factory([{packet, 1}])]].

compressed_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        fun () ->
            S1 = list_to_binary(lists:duplicate(200, $0)),
            S2 = list_to_binary(lists:duplicate(200, $1)),
            ?assertEqual(<<S1/binary, S2/binary>>,
                python:call(P, operator, add, [S1, S2]))
        end
    end} || Setup <- [setup_factory([{compressed, 9}]),
        setup3_factory([{compressed, 9}])]].

call_pipeline_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(N + 1, python:call(P, operator, add, [N , 1]))
            || N <- lists:seq(1, 50)]}
    end} || Setup <- [fun setup/0, fun setup3/0]].

queue_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(262144, python:call(P, test_utils, length,
                [<<0:262144/unit:8>>]))
            || _ <- lists:seq(1, 50)]}
    end} || Setup <- [fun setup/0, fun setup3/0]].

switch_test_() -> [{setup,
    fun () ->
        setup_event_logger(),
        Setup()
    end,
    fun (P) ->
        cleanup(P),
        cleanup_event_logger()
    end,
    fun (P) -> [
        fun () ->
            ?assertEqual(ok, python:switch(P, test_utils, switch, [5])),
            timer:sleep(500),
            ?assertEqual([
                {test_callback, 0, 0},
                {test_callback, 0, 1},
                {test_callback, 1, 2},
                {test_callback, 2, 3},
                {test_callback, 3, 4}
                ], get_events())
        end,
        fun () ->
            ?assertEqual(5, python:switch(P, test_utils, switch, [5],
                [wait_for_result])),
            ?assertEqual([
                {test_callback, 0, 0},
                {test_callback, 0, 1},
                {test_callback, 1, 2},
                {test_callback, 2, 3},
                {test_callback, 3, 4}
                ], get_events())
        end
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

integer_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, 0),
        ?_assertIdentity(P, 1),
        ?_assertIdentity(P, -1),
        ?_assertIdentity(P, 100000),
        ?_assertIdentity(P, -100000)
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

big_integer_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, 1000000000000),
        ?_assertIdentity(P, -1000000000000),
        ?_assertIdentity(P, 2 bsl 2040),
        ?_assertIdentity(P, -2 bsl 2040)
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

float_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, 0.0),
        ?_assertIdentity(P, 0.5),
        ?_assertIdentity(P, -0.5),
        ?_assertIdentity(P, 3.1415926),
        ?_assertIdentity(P, -3.1415926)
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

atom_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, ''),
        ?_assertIdentity(P, test),
        ?_assertIdentity(P, 'TEST'),
        ?_assertIdentity(P, true),
        ?_assertIdentity(P, false),
        ?_assertIdentity(P, undefined)
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

list_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, []),
        ?_assertIdentity(P, [0]),
        ?_assertIdentity(P, [1, 2, 3]),
        ?_assertIdentity(P, [[]]),
        ?_assertIdentity(P, [[], [], []]),
        ?_assertIdentity(P, lists:seq(1, 10000))
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

improper_list_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, [0 | 1]),
        ?_assertIdentity(P, [1, 2, 3 | 4]),
        ?_assertIdentity(P, [head | tail])
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

tuple_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, {}),
        ?_assertIdentity(P, {0}),
        ?_assertIdentity(P, {1, 2, 3}),
        ?_assertIdentity(P, {{}}),
        ?_assertIdentity(P, {{}, {}}),
        ?_assertIdentity(P, list_to_tuple(lists:seq(1, 10000)))
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

binary_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, <<>>),
        ?_assertIdentity(P, <<"test">>),
        ?_assertIdentity(P, <<0, 1, 2, 3, 4, 5>>)
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

opaque_type_test_() -> [{setup,
    Setup,
    fun cleanup/1,
    fun (P) -> [
        ?_assertIdentity(P, self()),
        ?_assertIdentity(P, [{pid, self()}]),
        ?_assertIdentity(P, <<1:2>>),
        fun () -> R = make_ref(), ?assertIdentity(P, R) end,
        ?_assertIdentity(P, fun erlang:is_atom/1),
        fun () -> F = fun () -> true end, ?assertIdentity(P, F) end
    ] end} || Setup <- [fun setup/0, fun setup3/0]].

%%%
%%% Utility functions
%%%

setup() ->
    (setup_factory([]))().

setup3() ->
    (setup3_factory([]))().

setup_factory(Options) ->
    fun () ->
        {ok, P} = python:start_link([{cd, "test/python"} | Options]),
        P
    end.

setup3_factory(Options) ->
    setup_factory([{python, "python3"} | Options]).

cleanup(P) ->
    ok = python:stop(P).

log_event(Event) ->
    true = ets:insert(events, {events, Event}).

get_events() ->
    Events = [E || {_, E} <- ets:lookup(events, events)],
    true = ets:delete(events, events),
    Events.

setup_event_logger() ->
    ets:new(events, [public, named_table, duplicate_bag]).

cleanup_event_logger() ->
    true = ets:delete(events).
