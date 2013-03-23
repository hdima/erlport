%%% Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
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
%%% @copyright 2009-2013 Dmitry Vasiliev <dima@hlabs.org>
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
    start_timer/2,
    stop_timer/1,
    try_send_request/4,
    handle_response/2
    ]).

-type timer() :: undefined | reference().

-define(is_allowed_term(T), (is_atom(T) orelse is_number(T)
    orelse is_binary(T))).

-include("erlport.hrl").


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

-spec start_timer(Timeout::pos_integer() | infinity, Message::term()) ->
    Timer::timer().

start_timer(infinity, _Message) ->
    undefined;
start_timer(Timeout, Message) when is_integer(Timeout) andalso Timeout > 0 ->
    erlang:send_after(Timeout, self(), Message).

%%
%% @doc Stop timer if needed
%%

-spec stop_timer(Timer::timer()) -> RemainigTime::non_neg_integer() | false.

stop_timer(undefined) ->
    false;
stop_timer(Timer) ->
    % TODO: Should we flush old messages as in gen_fsm?
    erlang:cancel_timer(Timer).

%%
%% @doc Try to send request
%%

try_send_request(From, Data, State=#state{queue=Queue}, Timeout) ->
    Timer = case From of
        unknown ->
            start_timer(Timeout, {erlport_timeout, out});
        _ ->
            start_timer(Timeout, {erlport_timeout, {out, From}})
    end,
    Info = {From, Timer},
    case send_or_queue(Info, Data, Queue, State) of
        wait ->
            Queue2 = queue:in({Info, Data}, Queue),
            {noreply, State#state{queue=Queue2}};
        Result ->
            Result
    end.

%%
%% @doc Handle response
%%

handle_response(Response, State=#state{sent=Sent}) ->
    case queue:out(Sent) of
        {{value, {From, Timer}}, Sent2} ->
            stop_timer(Timer),
            % TODO: Cleanup this code
            case From of
                unknown ->
                    case Response of
                        {error, _} ->
                            {stop, Response, State};
                        _ ->
                            process_queue(State#state{sent=Sent2})
                    end;
                _ ->
                    gen_server:reply(From, Response),
                    process_queue(State#state{sent=Sent2})
            end;
        {empty, Sent} ->
            {stop, orphan_response, State};
        _ ->
            {stop, unexpected_response, State}
    end.

%%=============================================================================
%% Utility functions
%%=============================================================================

send_request(Info, Data, Queue, State=#state{port=Port, sent=Sent}) ->
    case send_data(Port, Data) of
        ok ->
            Sent2 = queue:in(Info, Sent),
            {noreply, State#state{sent=Sent2, queue=Queue}};
        error ->
            {stop, port_closed, State}
    end.

process_queue(State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {noreply, State};
        {{value, Queued}, Queue2} ->
            send_from_queue(Queued, Queue2, State)
    end.

send_from_queue({Info, Data}, Queue, State) ->
    case send_or_queue(Info, Data, Queue, State) of
        wait ->
            {noreply, State};
        Result ->
            Result
    end.

send_or_queue(Info, Data, Queue, State=#state{port=Port, sent=Sent}) ->
    case queue:is_empty(Sent) of
        true ->
            send_request(Info, Data, Queue, State);
        false ->
            case try_send_data(Port, Data) of
                ok ->
                    Sent2 = queue:in(Info, Sent),
                    {noreply, State#state{sent=Sent2, queue=Queue}};
                wait ->
                    wait;
                error ->
                    {stop, port_closed, State}
            end
    end.
