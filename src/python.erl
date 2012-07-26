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
%%% @doc ErlPort Python interface
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2012 Dmitry Vasiliev <dima@hlabs.org>
%%%

-module(python).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-behaviour(gen_fsm).

-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    stop/1,
    call/4,
    cast/4,
    switch/4
    ]).

-export([
    init/1,
    client/2,
    client/3,
    server/2,
    server/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
    ]).

-record(state, {
    port :: port(),
    queue = queue:new() :: queue(),
    in_process :: {call, From::term(), Timer::term()} | {cast, Timer::term()}
    }).

-opaque instance() :: pid().

-define(START_TIMEOUT, 15000).
-define(CALL_TIMEOUT, 15000).

-include("erlport.hrl").


%%
%% @equiv start([])
%%

-spec start() -> Result when
    Result :: {ok, Instance} | {error, Reason},
    Instance :: instance(),
    Reason :: term().

start() ->
    start([]).

%%
%% @doc Start Python instance
%%

-spec start(Options) -> Result when
    Options :: erlport_options:options(),
    Result :: {ok, Instance} | {error, Reason},
    Instance :: instance(),
    Reason :: term().

start(Options) when is_list(Options) ->
    gen_fsm:start(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).

%%
%% @equiv start_link([])
%%

-spec start_link() -> Result when
    Result :: {ok, Instance} | {error, Reason},
    Instance :: instance(),
    Reason :: term().

start_link() ->
    start_link([]).

%%
%% @doc Start linked Python instance
%%

-spec start_link(Options) -> Result when
    Options :: erlport_options:options(),
    Result :: {ok, Instance} | {error, Reason},
    Instance :: instance(),
    Reason :: term().

start_link(Options) when is_list(Options) ->
    gen_fsm:start_link(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).

%%
%% @doc Stop Python instance
%%

-spec stop(Instance) -> ok when
    Instance :: instance().

stop(Instance) when is_pid(Instance) ->
    gen_fsm:send_all_state_event(Instance, stop).

%%
%% @doc Call Python function with arguments and return result
%%

-spec call(Instance, Module, Function, Args) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Result :: term().

% TODO: Call timeout?
call(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    case gen_fsm:sync_send_event(Instance, {call, Module, Function, Args}) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            erlang:error(Error)
    end.

%%
%% @doc Call Python function with arguments without waiting for result
%%

-spec cast(Instance, Module, Function, Args) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Result :: ok | {error, Reason},
    Reason :: term().

cast(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_fsm:sync_send_event(Instance, {cast, Module, Function, Args}).

%%
%% @doc Pass control to Python by calling functions with arguments
%%

-spec switch(Instance, Module, Function, Args) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Result :: ok | {error, Reason},
    Reason :: term().

switch(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_fsm:sync_send_event(Instance, {switch, Module, Function, Args}).


%%%
%%% Behaviour callbacks
%%%

%% @hidden

-spec init(Options) -> Result when
    Options :: erlport_options:options(),
    Result :: {ok, client, #state{}} | {stop, Reason},
    Reason :: term().

init(Options) when is_list(Options) ->
    % TODO: Add custom args?
    % TODO: Default call timeout?
    case erlport_options:parse(Options) of
        {ok, #options{python=Python,use_stdio=UseStdio,
                packet=Packet, port_options=PortOptions}} ->
            Path = lists:concat([Python,
                % Binary STDIO
                " -u",
                " -m erlport.cli",
                " --packet=", Packet,
                " --", UseStdio]),
            try open_port({spawn, Path}, PortOptions) of
                Port ->
                    {ok, client, #state{port=Port}}
            catch
                error:Error ->
                    {stop, {open_port_error, Error}}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @hidden

client({call, Module, Function, Args}, From, State=#state{})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'C', Module, Function, Args}),
    try_to_request(client, State, Data, {call, From});
client({cast, Module, Function, Args}, _From, State=#state{})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'M', Module, Function, Args}),
    case try_to_request(client, State, Data, cast) of
        {next_state, StateName, State2} ->
            {reply, ok, StateName, State2};
        Response ->
            Response
    end;
client({switch, Module, Function, Args}, _From, State=#state{})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'S', Module, Function, Args}),
    case try_to_request(server, State, Data, switch) of
        {next_state, StateName, State2} ->
            {reply, ok, StateName, State2};
        Response ->
            Response
    end;
client(Event, From, State) ->
    {reply, {bad_event, ?MODULE, Event, From}, client, State}.

%% @hidden

client(timeout, State=#state{in_process=InProcess}) ->
    case InProcess of
        {call, From, _} ->
            gen_fsm:reply(From, {error, timeout}),
            {stop, timeout, State};
        _ ->
            {next_state, client, State}
    end;
client(_Event, State) ->
    {next_state, client, State}.

%% @hidden

server(_Event, _From, State) ->
    {reply, {error, server_mode}, server, State}.

%% @hidden

server(_Event, State) ->
    {next_state, server, State}.

%% @hidden

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @hidden

handle_info({Port, {data, Data}}, StateName=client, State=#state{port=Port,
        in_process=InProcess}) ->
    try binary_to_term(Data) of
        'R' ->
            case InProcess of
                undefined ->
                    {stop, orphan_response, State};
                {cast, Timer} ->
                    gen_fsm:cancel_timer(Timer),
                    check_queue(StateName, State);
                {call, _From, _Timer} ->
                    {stop, unexpected_response, State}
            end;
        {'R', Result} ->
            case InProcess of
                undefined ->
                    {stop, orphan_response, State};
                {call, From, Timer} ->
                    gen_fsm:cancel_timer(Timer),
                    gen_fsm:reply(From, Result),
                    check_queue(StateName, State);
                {cast, _Timer} ->
                    {stop, unexpected_response, State}
            end;
        _ ->
            % Ignore possible call requests
            {next_state, StateName, State}
    catch
        error:badarg ->
            case InProcess of
                {call, From, _Timer} ->
                    gen_fsm:reply(From, {error, invalid_response});
                _ ->
                    ok
            end,
            {stop, invalid_response, State}
    end;
handle_info({Port, {data, Data}}, StateName=server, State=#state{port=Port,
        queue=Queue}) ->
    % TODO: Check queue for responses
    try binary_to_term(Data) of
        {'C', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            proc_lib:spawn(fun () ->
                Result = try {ok, apply(Module, Function, Args)}
                    catch
                        Class:Reason ->
                            {error, Class, Reason, erlang:get_stacktrace()}
                    end,
                Response = {'R', Result},
                true = port_command(Port, term_to_binary(Response))
                end),
            % TODO: Monitor process?
            {next_state, StateName, State};
        {'M', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            proc_lib:spawn(fun () -> apply(Module, Function, Args) end),
            Data = term_to_binary('R'),
            case try_to_send(Port, Data, false) of
                ok ->
                    {next_state, StateName, State};
                wait ->
                    Queue2 = queue:in({response, Data}, Queue),
                    {next_state, StateName, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            {next_state, StateName, State}
    catch
        error:badarg ->
            {stop, invalid_request, State}
    end;
% TODO: More port related handlers
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @hidden

handle_sync_event(Event, From, StateName, State) ->
    {reply, {bad_event, ?MODULE, Event, From}, StateName, State}.

%% @hidden

terminate(_Reason, _StateName, _State) ->
    ok.

%% @hidden

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%
%%% Utility functions
%%%

try_to_send(Port, Data) ->
    try_to_send(Port, Data, true).

try_to_send(Port, Data, WithTimer) ->
    try port_command(Port, Data, [nosuspend]) of
        true ->
            case WithTimer of
                true ->
                    Timer = gen_fsm:send_event_after(?CALL_TIMEOUT, timeout),
                    {ok, Timer};
                false ->
                    ok
            end;
        false ->
            wait
    catch
        error:badarg ->
            fail
    end.

check_queue(StateName=client, State=#state{port=Port, queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State#state{in_process=undefined}};
        {{value, {call, Client, Data}}, Queue2} ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {call, Client, Timer},
                    {next_state, StateName, State#state{in_process=Info,
                        queue=Queue2}};
                wait ->
                    {next_state, StateName, State#state{in_process=undefined}};
                fail ->
                    {stop, port_closed, State}
            end;
        {{value, {cast, Data}}, Queue2} ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {cast, Timer},
                    {next_state, StateName, State#state{in_process=Info,
                        queue=Queue2}};
                wait ->
                    {next_state, StateName, State#state{in_process=undefined}};
                fail ->
                    {stop, port_closed, State}
            end
    end;
check_queue(StateName, State=#state{port=Port, queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State#state{in_process=undefined}};
        {{value, {response, Data}}, Queue2} ->
            case try_to_send(Port, Data, false) of
                ok ->
                    {next_state, StateName, State#state{queue=Queue2}};
                wait ->
                    {next_state, StateName, State};
                fail ->
                    {stop, port_closed, State}
            end
    end.

try_to_request(StateName, State=#state{port=Port, queue=Queue,
        in_process=InProcess}, Data, Type) ->
    case InProcess of
        undefined ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = request(Type, Timer),
                    {next_state, StateName, State#state{in_process=Info}};
                wait ->
                    Queue2 = queue:in(queued(Type, Data), Queue),
                    {next_state, StateName, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            Queue2 = queue:in(queued(Type, Data), Queue),
            {next_state, StateName, State#state{queue=Queue2}}
    end.


request({call, From}, Timer) ->
    {call, From, Timer};
request(cast, Timer) ->
    {cast, Timer};
request(switch, Timer) ->
    {switch, Timer}.

queued({call, From}, Data) ->
    {call, From, Data};
queued(cast, Data) ->
    {cast, Data};
queued(switch, Data) ->
    {switch, Data}.
