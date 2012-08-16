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

-include_lib("eunit/include/eunit.hrl").

-export([test_callback/2]).


test_callback(PrevResult, N) ->
    log_event({test_callback, PrevResult, N}),
    N.

setup() ->
    {ok, P} = python:start_link([{cd, "test/python"}]),
    P.

cleanup(P) ->
    ok = python:stop(P).

call_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) ->
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    end}.

compressed_test_() -> {setup,
    fun () ->
        {ok, P} = python:start_link([{compressed, 9}]),
        P
    end,
    fun cleanup/1,
    fun (P) ->
        fun () ->
            S1 = list_to_binary(lists:duplicate(200, $0)),
            S2 = list_to_binary(lists:duplicate(200, $1)),
            ?assertEqual(<<S1/binary, S2/binary>>,
                python:call(P, operator, add, [S1, S2]))
        end
    end}.

call_queue_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(N + 1, python:call(P, operator, add, [N , 1]))
            || N <- lists:seq(1, 50)]}
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
            ?assertEqual(ok, python:switch(P, switch, switch, [5])),
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
            ?assertEqual(5, python:switch(P, switch, switch, [5], [block])),
            ?assertEqual([
                {test_callback, 0, 0},
                {test_callback, 0, 1},
                {test_callback, 1, 2},
                {test_callback, 2, 3},
                {test_callback, 3, 4}
                ], get_events())
        end
    ] end}.

data_types_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) -> [
        ?_assertEqual(0, python:call(P, identity, identity, [0])),
        ?_assertEqual(1, python:call(P, identity, identity, [1])),
        ?_assertEqual(-1, python:call(P, identity, identity, [-1])),
        ?_assertEqual(1000000000,
            python:call(P, identity, identity, [1000000000])),
        ?_assertEqual(-1000000000,
            python:call(P, identity, identity, [-1000000000])),
        ?_assertEqual(0.0, python:call(P, identity, identity, [0.0])),
        ?_assertEqual(0.5, python:call(P, identity, identity, [0.5])),
        ?_assertEqual(-0.5, python:call(P, identity, identity, [-0.5])),
        ?_assertEqual('', python:call(P, identity, identity, [''])),
        ?_assertEqual('test', python:call(P, identity, identity, ['test'])),
        ?_assertEqual('true', python:call(P, identity, identity, ['true'])),
        ?_assertEqual('false', python:call(P, identity, identity, ['false'])),
        ?_assertEqual('undefined',
            python:call(P, identity, identity, ['undefined'])),
        ?_assertEqual([], python:call(P, identity, identity, [[]])),
        ?_assertEqual([1, 2, 3],
            python:call(P, identity, identity, [[1, 2, 3]])),
        ?_assertEqual([[], []], python:call(P, identity, identity, [[[], []]])),
        % TODO: Doesn't work as expected
        %?_assertEqual([0|1], python:call(P, identity, identity, [0|1])),
        ?_assertEqual({}, python:call(P, identity, identity, [{}])),
        ?_assertEqual({{}, {}}, python:call(P, identity, identity, [{{}, {}}])),
        ?_assertEqual(<<>>, python:call(P, identity, identity, [<<>>])),
        ?_assertEqual(<<"test">>,
            python:call(P, identity, identity, [<<"test">>])),
        ?_assertEqual(self(), python:call(P, identity, identity, [self()]))
    ] end}.

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
