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

-module(ruby_tests).

-export([test_callback/2]).

-include_lib("eunit/include/eunit.hrl").


test_callback(PrevResult, N) ->
    log_event({test_callback, PrevResult, N}),
    N.

start_stop_test_() -> [
    fun () ->
        {ok, P} = ruby:start(),
        ?assertEqual(ok, ruby:stop(P))
    end,
    fun () ->
        {ok, P} = ruby:start_link(),
        ?assertEqual(ok, ruby:stop(P))
    end,
    fun () ->
        ?assertMatch({ok, _}, ruby:start({local, ruby_test})),
        ?assertEqual(ok, ruby:stop(ruby_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, ruby:start_link({local, ruby_test})),
        ?assertEqual(ok, ruby:stop(ruby_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, ruby:start({local, ruby_test}, [])),
        ?assertEqual(ok, ruby:stop(ruby_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, ruby:start_link({local, ruby_test}, [])),
        ?assertEqual(ok, ruby:stop(ruby_test))
    end
    ].

call_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (R) ->
        ?_assertEqual(4, ruby:call(R, '', 'Kernel::eval', [<<"2 + 2">>]))
    end}.

call_back_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (R) -> [
        ?_assertError({ruby, 'InvalidMode',
                <<"call() is unsupported in server mode">>, [_|_]},
            ruby:call(R, 'erlport/erlang', 'Erlang::call',
                [erlang, length, [[1, 2, 3]]])),
        ?_assertEqual(3, ruby:switch(R, 'erlport/erlang', 'Erlang::call',
            [erlang, length, [[1, 2, 3]]], [wait_for_result]))
    ] end}.

error_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (R) -> [
        ?_assertError({ruby, 'LoadError',
                <<"no such file to load -- unknown">>, [_|_]},
            ruby:call(R, unknown, unknown, [])),
        ?_assertError({ruby, 'CallError',
                <<"Tuple([:erlang, :error, :undef, "
                "[Tuple([:unknown, :unknown, []]), ", _/binary>>, [_|_]},
            ruby:switch(R, 'erlport/erlang', 'Erlang::call',
                [unknown, unknown, []], [wait_for_result])),
        fun () ->
            R2 = setup(),
            try
                ?assertError({ruby, 'CallError',
                        <<"Tuple([:ruby, :LoadError, "
                        "\"no such file to load -- unknown\", ",
                        _/binary>>, [_|_]},
                    ruby:switch(R, 'erlport/erlang', 'Erlang::call',
                        [ruby, call, [R2, unknown, unknown, []]],
                        [wait_for_result]))
            after
                cleanup(R2)
            end
        end
    ] end}.

stdin_stdout_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (R) -> [
        ?_test(erlport_test_utils:assert_output(<<"HELLO!\n">>,
            fun () -> undefined = ruby:call(R, '', 'ARGF::puts',
                [<<"HELLO!">>]) end, R)),
        ?_assertError({ruby, 'IOError',
            <<"STDIN is closed for ErlPort connected process">>, [_|_]},
            ruby:call(R, '', 'ARGF::read', []))
    ] end}.

nouse_stdio_test_() ->
    case os:type() of
        {win32, _} ->
            [];
        _ ->
            {setup,
                setup_factory([nouse_stdio]),
                fun cleanup/1,
                fun (R) ->
                    ?_assertEqual(4, ruby:call(R, '', 'Kernel::eval',
                        [<<"2 + 2">>]))
                end}
    end.

packet4_test_() -> {setup,
    setup_factory([{packet, 4}]),
    fun cleanup/1,
    fun (R) ->
        ?_assertEqual(4, ruby:call(R, '', 'Kernel::eval', [<<"2 + 2">>]))
    end}.

packet2_test_() -> {setup,
    setup_factory([{packet, 2}]),
    fun cleanup/1,
    fun (R) ->
        ?_assertEqual(4, ruby:call(R, '', 'Kernel::eval', [<<"2 + 2">>]))
    end}.

packet1_test_() -> {setup,
    setup_factory([{packet, 1}]),
    fun cleanup/1,
    fun (R) ->
        ?_assertEqual(4, ruby:call(R, '', 'Kernel::eval', [<<"2 + 2">>]))
    end}.

compressed_test_() -> {setup,
    setup_factory([{compressed, 9}]),
    fun cleanup/1,
    fun (R) ->
        fun () ->
            S1 = list_to_binary(lists:duplicate(200, $0)),
            S2 = list_to_binary(lists:duplicate(200, $1)),
            ?assertEqual(<<S1/binary, S2/binary>>,
                ruby:call(R, test_utils, 'Test::add', [S1, S2]))
        end
    end}.

call_pipeline_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(N + 1, ruby:call(P, test_utils, 'Test::add', [N , 1]))
            || N <- lists:seq(1, 50)]}
    end}.

queue_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(262144, ruby:call(P, test_utils, 'Test::len',
                [<<0:262144/unit:8>>]))
            || _ <- lists:seq(1, 50)]}
    end}.

switch_test_() -> {setup,
    fun () ->
        setup_event_logger(),
        setup()
    end,
    fun (P) ->
        cleanup(P),
        cleanup_event_logger()
    end,
    fun (P) -> [
        fun () ->
            ?assertEqual(ok, ruby:switch(P, test_utils, switch, [5])),
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
            ?assertEqual(5, ruby:switch(P, test_utils, switch, [5],
                [wait_for_result])),
            ?assertEqual([
                {test_callback, 0, 0},
                {test_callback, 0, 1},
                {test_callback, 1, 2},
                {test_callback, 2, 3},
                {test_callback, 3, 4}
                ], get_events())
        end
    ] end}.

%%%
%%% Utility functions
%%%

setup() ->
    (setup_factory([]))().

setup_factory(Options) ->
    fun () ->
        {ok, P} = ruby:start_link([{cd, "test/ruby"} | Options]),
        P
    end.

cleanup(P) ->
    ok = ruby:stop(P).

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
