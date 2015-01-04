%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
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

-module(python3_tests).

-export([test_callback/1, recurse/2]).

-include_lib("eunit/include/eunit.hrl").

-define(SETUP(Setup, Tests), {setup,
    Setup,
    fun cleanup/1,
    fun (P) ->
        Tests
    end}).
-define(SETUP(Tests), ?SETUP(fun setup/0, Tests)).

-define(TIMEOUT, 5000).


test_callback(Result) ->
    log_event({test_callback, Result}),
    Result.

recurse(P, N) ->
    python:call(P, test_utils, recurse, [P, N]).

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

call_test_() ->
    ?SETUP(
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    ).

python_cast_test_() ->
    ?SETUP(
        fun () ->
            Pid = self(),
            Message = test_message,
            ?assertEqual(undefined, python:call(P, 'erlport.erlang', cast,
                [Pid, Message])),
            ?assertEqual(ok, receive
                    Message ->
                        ok
                after
                    ?TIMEOUT ->
                        timeout
                end)
        end
    ).

erlang_cast_test_() -> {setup,
    fun () ->
        setup_event_logger(),
        setup()
    end,
    fun (P) ->
        cleanup(P),
        cleanup_event_logger()
    end,
    fun (P) ->
        fun () ->
            ?assertEqual(ok, python:call(P, test_utils, setup_message_handler,
                [])),
            ?assertEqual(ok, python:cast(P, test_message)),
            P ! test_message2,
            timer:sleep(500),
            ?assertEqual([{test_callback, {message, test_message}},
                {test_callback, {message, test_message2}}], get_events())
        end
    end}.

message_handler_error_test_() -> {setup,
    fun () ->
        ok = error_logger:tty(false)
    end,
    fun (_) ->
        ok = error_logger:tty(true)
    end,
    fun () ->
        P = setup(),
        process_flag(trap_exit, true),
        try
            link(P),
            ?assertEqual(ok, python:call(P, test_utils,
                setup_faulty_message_handler, [])),
            P ! test_message,
            ?assertEqual(ok,
                receive
                    {'EXIT', P, {message_handler_error,
                            {python, 'builtins.ValueError',
                                "b'test_message'", [_|_]}}} ->
                        ok
                after
                    3000 ->
                        timeout
                end)
        after
            process_flag(trap_exit, false)
        end
    end}.

async_call_error_test_() -> {setup,
    fun () ->
        ok = error_logger:tty(false)
    end,
    fun (_) ->
        ok = error_logger:tty(true)
    end,
    fun () ->
        P = setup(),
        process_flag(trap_exit, true),
        try
            link(P),
            python:call(P, unknown, unknown, [], [async]),
            ?assertEqual(ok,
                receive
                    {'EXIT', P, {async_call_error,
                            {python, 'builtins.ImportError',
                                "No module named unknown", [_|_]}}} ->
                        ok;
                    {'EXIT', P, {async_call_error,
                            {python, 'builtins.ImportError',
                                "No module named 'unknown'", [_|_]}}} ->
                        ok
                after
                    3000 ->
                        timeout
                end)
        after
            process_flag(trap_exit, false)
        end
    end}.

recursion_test_() ->
    ?SETUP(
        ?_assertEqual(done, python:call(P, test_utils, recurse, [P, 50]))
    ).


objects_hierarchy_test_() ->
    ?SETUP(
        ?_assertEqual(ok, python:call(P, test_utils,
            'TestClass.TestSubClass.test_method', []))
    ).

erlang_util_functions_test_() ->
    ?SETUP([
        fun () ->
            ?assertEqual(P, python:call(P, 'erlport.erlang', self, [])),
            % Check cached value
            ?assertEqual(P, python:call(P, 'erlport.erlang', self, []))
        end,
        fun () ->
            Ref = python:call(P, 'erlport.erlang', make_ref, []),
            ?assert(is_reference(Ref)),
            Ref2 = python:call(P, 'erlport.erlang', make_ref, []),
            ?assert(is_reference(Ref2)),
            ?assertNot(Ref =:= Ref2)
        end
    ]).

error_test_() ->
    ?SETUP([
        ?_assertEqual({error, python},
            try python:call(P, unknown, unknown, [])
            catch
                error:{python, 'builtins.ImportError',
                        "No module named unknown", [_|_]} ->
                    {error, python};
                error:{python, 'builtins.ImportError',
                        "No module named 'unknown'", [_|_]} ->
                    {error, python}
            end),
        ?_assertError({python, 'erlport.erlang.CallError',
                "(Atom(b'erlang'), Atom(b'error'), Atom(b'undef'), "
                "List([(Atom(b'unknown'), Atom(b'unknown'), List([])" ++ _,
                [_|_]},
            python:call(P, 'erlport.erlang', call, [unknown, unknown, []])),
        fun () ->
            P2 = setup(),
            try
                ?assertError({python, 'erlport.erlang.CallError',
                        "(Atom(b'python'), Atom(b'builtins.ImportError'), "
                        ++ _, [_|_]},
                    python:call(P, 'erlport.erlang', call,
                        [python, call, [P2, unknown, unknown, []]]))
            after
                cleanup(P2)
            end
        end
    ]).

stdin_stdout_test_() ->
    ?SETUP([
        ?_test(erlport_test_utils:assert_output(<<"HELLO!\n">>,
            fun () -> undefined = python:call(P, test_utils, print_string,
                ["HELLO!"]) end, P)),
        ?_test(erlport_test_utils:assert_output(
            <<16#d0, 16#9f, 16#d1, 16#80, 16#d0, 16#b8, 16#d0, 16#b2,
                16#d0, 16#b5, 16#d1, 16#82, "!\n">>,
            fun () -> undefined = python:call(P, test_utils, print_string,
                [[16#41f, 16#440, 16#438, 16#432, 16#435, 16#442, $!]])
                end, P)),
        ?_assertError({python, 'io.UnsupportedOperation', "readline", [_|_]},
            python:call(P, builtins, input, []))
    ]).

nouse_stdio_test_() ->
    case os:type() of
        {win32, _} ->
            [];
        _ ->
            ?SETUP(
                setup_factory([nouse_stdio]),
                ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
            )
    end.

packet4_test_() ->
    ?SETUP(
        setup_factory([{packet, 4}]),
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    ).

packet2_test_() ->
    ?SETUP(
        setup_factory([{packet, 2}]),
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    ).

packet1_test_() ->
    ?SETUP(
        setup_factory([{packet, 1}]),
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    ).

compressed_test_() ->
    ?SETUP(
        setup_factory([{compressed, 9}]),
        fun () ->
            S1 = list_to_binary(lists:duplicate(200, $0)),
            S2 = list_to_binary(lists:duplicate(200, $1)),
            ?assertEqual(<<S1/binary, S2/binary>>,
                python:call(P, operator, add, [S1, S2]))
        end
    ).

call_pipeline_test_() ->
    ?SETUP(
        {inparallel, [
            ?_assertEqual(N + 1, python:call(P, operator, add, [N , 1]))
            || N <- lists:seq(1, 50)]}
    ).

queue_test_() ->
    ?SETUP(
        {inparallel, [
            ?_assertEqual(262144, python:call(P, test_utils, length,
                [<<0:262144/unit:8>>]))
            || _ <- lists:seq(1, 50)]}
    ).

call_back_test_() -> {setup,
    fun () ->
        setup_event_logger(),
        setup()
    end,
    fun (P) ->
        cleanup(P),
        cleanup_event_logger()
    end,
    fun (P) -> [
        ?_assertEqual(3, python:call(P, 'erlport.erlang', call,
            [erlang, length, [[1, 2, 3]]])),
        fun () ->
            ?assertEqual(ok, python:call(P, test_utils, switch, [5], [async])),
            timer:sleep(500),
            ?assertEqual([
                {test_callback, {0, 0}},
                {test_callback, {0, 1}},
                {test_callback, {1, 2}},
                {test_callback, {2, 3}},
                {test_callback, {3, 4}}
                ], get_events())
        end,
        fun () ->
            ?assertEqual(5, python:call(P, test_utils, switch, [5])),
            ?assertEqual([
                {test_callback, {0, 0}},
                {test_callback, {0, 1}},
                {test_callback, {1, 2}},
                {test_callback, {2, 3}},
                {test_callback, {3, 4}}
                ], get_events())
        end
    ] end}.

datatype_test_() ->
    ?SETUP(
        [?_assertEqual(V, python:call(P, test_utils, identity, [V]))
            || V <- datatype_test_data:get_test_data()]
    ).

custom_datatype_test_() ->
    ?SETUP(
        fun () ->
            ?assertEqual(ok, python:call(P, test_utils, setup_date_types, [])),
            ?assertEqual({date, {2013, 2, 1}}, python:call(P, operator, add,
                [{date, {2013, 1, 31}}, {days, 1}]))
        end
    ).

%%%
%%% Utility functions
%%%

setup() ->
    (setup_factory([]))().

setup_factory(Options) ->
    fun () ->
            {ok, P} = python:start_link([{python_path, "test/python3"}
                                         | Options]),
        P
    end.

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
