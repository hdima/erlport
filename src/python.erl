%%% Copyright (c) 2009, 2010, Dmitry Vasiliev <dima@hlabs.org>
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

-module(python).

-behaviour(gen_server).

-export([
    start/0,
    start_link/0,
    stop/1,
    call/4,
    cast/4
    ]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-record(state, {
    port :: port()
    }).


start() ->
    gen_server:start(?MODULE, undefined, []).


start_link() ->
    gen_server:start_link(?MODULE, undefined, []).


stop(InstanceId) when is_pid(InstanceId) ->
    gen_server:cast(InstanceId, stop).


call(InstanceId, Module, Function, Args) when is_pid(InstanceId),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_server:call(InstanceId, {call, Module, Function, Args}).


cast(InstanceId, Module, Function, Args) when is_pid(InstanceId),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_server:cast(InstanceId, {call, Module, Function, Args}).


%%%
%%% Behaviour callbacks
%%%

init(undefined) ->
    Port = open_port({spawn, "python -u -m erlport.cli"},
        [{packet, 4}, binary, use_stdio]),
    {ok, #state{port=Port}}.


handle_call({call, Module, Function, Args}, From, State=#state{port=Port})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Request = {'S', From, Module, Function, Args},
    true = port_command(Port, term_to_binary(Request)),
    % TODO: Need timeout
    {noreply, State};
handle_call(Request, From, State) ->
    {reply, {error, {badarg, ?MODULE, Request, From}}, State}.


handle_cast({call, Module, Function, Args}, State=#state{port=Port})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Request = {'A', Module, Function, Args},
    true = port_command(Port, term_to_binary(Request)),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({Port, {data, Data}}, State=#state{port=Port}) ->
    % TODO: Check binary_to_term errors
    case binary_to_term(Data) of
        {'R', From, Result} ->
            gen_server:reply(From, Result),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
