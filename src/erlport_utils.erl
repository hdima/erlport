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

%%%
%%% @doc ErlPort utility functions
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2012 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(erlport_utils).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    send_data/2,
    try_send_data/2,
    encode_term/2,
    prepare_term/1,
    prepare_list/1,
    start_timer/1,
    stop_timer/1
    ]).

-type timer() :: undefined | reference().

-define(is_allowed_term(T), (is_atom(T) orelse is_number(T)
    orelse is_binary(T))).


%%
%% @doc Send data to port
%%

-spec send_data(Port::port(), Data::binary()) -> ok | error.

send_data(Port, Data) when is_port(Port) andalso is_binary(Data) ->
    try port_command(Port, Data) of
        true ->
            ok
    catch
        error:badarg ->
            error
    end.

%%
%% @doc Try to send data to port
%%

-spec try_send_data(Port::port(), Data::binary()) -> ok | wait | error.

try_send_data(Port, Data) when is_port(Port) andalso is_binary(Data) ->
    try erlang:port_command(Port, Data, [nosuspend]) of
        true ->
            ok;
        false ->
            wait
    catch
        error:badarg ->
            error
    end.

%%
%% @doc Encode Erlang term
%%

-spec encode_term(Term::term(), Compressed::0..9) -> Data::binary().

encode_term(Term, Compressed) when is_integer(Compressed)
        andalso Compressed >= 0 andalso Compressed =< 9 ->
    term_to_binary(Term, [{minor_version, 1}, {compressed, Compressed}]).

%%
%% @doc Prepare Erlang term for encoding
%%

-spec prepare_term(Term::term()) -> PreparedTerm::term().

prepare_term(Term) ->
    if
        ?is_allowed_term(Term) ->
            Term;
        is_list(Term) ->
            prepare_list(Term);
        is_tuple(Term) ->
            list_to_tuple(prepare_list(tuple_to_list(Term)));
        true ->
            <<131, Data/binary>> = term_to_binary(Term, [{minor_version, 1}]),
            {'$erlport.opaque', erlang, Data}
    end.

%%
%% @doc Prepare Erlang list for encoding
%%

-spec prepare_list(List::list() | term()) -> PreparedList::list().

prepare_list([Item | Tail]) ->
    [prepare_term(Item) | prepare_list(Tail)];
prepare_list([]) ->
    [];
prepare_list(ImproperTail) ->
    prepare_term(ImproperTail).

%%
%% @doc Start timer if needed
%%

-spec start_timer(Timeout::erlport_options:call_timeout()) -> Timer::timer().

start_timer(infinity) ->
    undefined;
start_timer(Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    gen_fsm:send_event_after(Timeout, timeout).

%%
%% @doc Stop timer if needed
%%

-spec stop_timer(Timer::timer()) -> RemainigTime::non_neg_integer() | false.

stop_timer(undefined) ->
    false;
stop_timer(Timer) ->
    gen_fsm:cancel_timer(Timer).
