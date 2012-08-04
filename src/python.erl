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

-compile(inline).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-behaviour(gen_fsm).

-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    stop/1,
    call/4,
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
    % TODO: Rename to 'waiting'
    queue = queue:new() :: queue(),
    % TODO: Rename to 'sent' and convert to queue
    in_process :: {call | switch, From::term(), Timer::term()},
    call :: {Monitor::reference(), Timer::reference(), Pid::pid()}
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
%% @doc Pass control to Python by calling the function with arguments
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

%%
%% @doc Process initialization callback
%% @hidden
%%

-spec init(Options) -> Result when
    Options :: erlport_options:options(),
    Result :: {ok, client, #state{}} | {stop, Reason},
    Reason :: term().

init(Options) when is_list(Options) ->
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

%%
%% @doc Synchronous event handler in client mode
%% @hidden
%%

-spec client(Event, From, State) -> Response when
    Event :: {Type, Module, Function, Args},
    Type :: call | switch,
    Module :: atom(),
    Function :: atom(),
    Args :: [term()],
    From :: {Pid, Tag},
    Pid :: pid(),
    Tag :: reference(),
    Response :: {reply, Result, StateName, State}
        | {next_state, StateName, State}
        | {stop, port_closed, State},
    Result :: term(),
    StateName :: atom(),
    State :: #state{}.

client({call, Module, Function, Args}, From, State=#state{})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = encode({'C', Module, Function, Args}),
    send_request({call, From, Data}, client, State);
client({switch, Module, Function, Args}, From, State=#state{})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = encode({'S', Module, Function, Args}),
    send_request({switch, From, Data}, server, State);
client(Event, From, State) ->
    {reply, {unknown_event, ?MODULE, Event, From}, client, State}.

%%
%% @doc Asynchronous event handler in client mode
%% @hidden
%%

-spec client(Event, State) -> Response when
    Event :: timeout,
    State :: #state{},
    Response :: {next_state, client, State}
        | {stop, timeout, State}.

client(timeout, State=#state{}) ->
    error_response(timeout, State);
client(_Event, State) ->
    {next_state, client, State}.

%%
%% @doc Synchronous event handler in server mode
%% @hidden
%%

-spec server(Event, From, State) -> Response when
    Event :: term(),
    From :: {Pid, Tag},
    Pid :: pid(),
    Tag :: reference(),
    State :: #state{},
    Response :: {reply, {error, server_mode}, server, State}.

server(_Event, _From, State) ->
    {reply, {error, server_mode}, server, State}.

%%
%% @doc Asynchronous event handler in server mode
%% @hidden
%%

-spec server(Event, State) -> Response when
    Event :: timeout,
    State :: #state{},
    Response :: {next_state, server, State}.

server(timeout, State=#state{port=Port}) ->
    % TODO: We need to add request ID
    Data = encode({'e', {error, timeout, []}}),
    case send_data(Port, Data, false) of
        ok ->
            {next_state, server, State#state{call=undefined}};
        fail ->
            {stop, port_closed, State#state{call=undefined}}
    end;
server(_Event, State) ->
    {next_state, server, State}.

%%
%% @doc Generic asynchronous event handler
%% @hidden
%%

-spec handle_event(Event, StateName, State) -> Response when
    Event :: stop,
    StateName :: atom(),
    State :: #state{},
    Response :: {stop, normal, State} | {next_state, StateName, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @hidden

handle_info({Port, {data, Data}}, StateName=client, State=#state{port=Port}) ->
    try binary_to_term(Data) of
        {'r', Result} ->
            handle_response(call, {ok, Result}, State, StateName);
        {'e', Error} ->
            handle_response(call, {error, Error}, State, StateName);
        Response ->
            error_response({invalid_response, Response}, State)
    catch
        error:badarg ->
            error_response({invalid_response_data, Data}, State)
    end;
handle_info({Port, {data, Data}}, StateName=server, State=#state{port=Port}) ->
    try binary_to_term(Data) of
        's' ->
            handle_response(switch, ok, State, StateName);
        {'C', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            Timer = gen_fsm:send_event_after(?CALL_TIMEOUT, timeout),
            % FIXME: proc_lib:spawn and monitor?
            {Pid, Monitor} = spawn_monitor(fun () ->
                exit(try {ok, apply(Module, Function, Args)}
                    catch
                        Class:Reason ->
                            {error, {Class, Reason, erlang:get_stacktrace()}}
                    end)
                end),
            Info = {Monitor, Timer, Pid},
            {next_state, StateName, State#state{call=Info}};
        {'r', _Result} ->
            % TODO: Result will be handled when we'll support switch result
            % waiting
            {next_state, client, State};
        {'e', Error} ->
            % TODO: Unpack error if needed
            {stop, {switch_failed, Error}, State};
        Request ->
            {stop, {invalid_request, Request}, State}
    catch
        error:badarg ->
            {stop, {invalid_request_data, Data}, State}
    end;
handle_info({'DOWN', Monitor, process, Pid, Result}, StateName=server,
        State=#state{port=Port, call={Monitor, Timer, Pid}}) ->
    gen_fsm:cancel_timer(Timer),
    R = case Result of
        {ok, Response} ->
            {'r', Response};
        {error, Response} ->
            {'e', Response};
        Response ->
            {'e', {error, Response, []}}
    end,
    case send_data(Port, encode(R), false) of
        ok ->
            {next_state, StateName, State#state{call=undefined}};
        fail ->
            {stop, port_closed, State#state{call=undefined}}
    end;
handle_info({Port, closed}, _StateName, State=#state{port=Port}) ->
    {stop, port_closed, State};
handle_info({'EXIT', Port, Reason}, _StateName, State=#state{port=Port}) ->
    {stop, {port_closed, Reason}, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @hidden

handle_sync_event(Event, From, StateName, State) ->
    {reply, {unknown_event, ?MODULE, Event, From}, StateName, State}.

%% @hidden

terminate(Reason, _StateName, #state{in_process=InProcess, queue=Queue}) ->
    Error = {error, Reason},
    case InProcess of
        undefined ->
            ok;
        {_Type, From, _Timer} ->
            gen_fsm:reply(From, Error)
    end,
    foreach_in_queue(fun ({_Type, From, _Data}) ->
        gen_fsm:reply(From, Error)
        end, Queue).

%% @hidden

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%
%%% Auxiliary functions
%%%

send_data(Port, Data, WithTimer) ->
    try {port_command(Port, Data), WithTimer} of
        {true, true} ->
            {ok, gen_fsm:send_event_after(?CALL_TIMEOUT, timeout)};
        {true, false} ->
            ok
    catch
        error:badarg ->
            fail
    end.

process_queue(StateName=client, State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State};
        {{value, Queued}, Queue2} ->
            send_from_queue(Queued, Queue2, StateName, State)
    end;
process_queue(StateName=server, State=#state{port=Port, queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State#state{in_process=undefined}};
        {{value, {response, Data}}, Queue2} ->
            case send_data(Port, Data, false) of
                ok ->
                    {next_state, StateName, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end
    end.

send_request(Queued={Type, From, Data}, StateName, State=#state{port=Port,
        queue=Queue, in_process=InProcess}) ->
    case InProcess of
        undefined ->
            case send_data(Port, Data, true) of
                {ok, Timer} ->
                    Info = {Type, From, Timer},
                    {next_state, StateName, State#state{in_process=Info}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            Queue2 = queue:in(Queued, Queue),
            {next_state, StateName, State#state{queue=Queue2}}
    end.

send_from_queue({Type, From, Data}, Queue, StateName,
        State=#state{port=Port}) ->
    case send_data(Port, Data, true) of
        {ok, Timer} ->
            Info = {Type, From, Timer},
            {next_state, StateName, State#state{in_process=Info, queue=Queue}};
        fail ->
            {stop, port_closed, State}
    end.

handle_response(ExpectedType, Response, State=#state{in_process=InProcess},
        StateName) ->
    case InProcess of
        {ExpectedType, From, Timer} ->
            gen_fsm:cancel_timer(Timer),
            gen_fsm:reply(From, Response),
            process_queue(StateName, State#state{in_process=undefined});
        undefined ->
            {stop, orphan_response, State};
        _ ->
            {stop, unexpected_response, State}
    end.

error_response(Error, State=#state{in_process=InProcess}) ->
    case InProcess of
        {_Type, From, _Timer} ->
            gen_fsm:reply(From, {error, Error});
        undefined ->
            ok
    end,
    {stop, Error, State}.

encode(Term) ->
    term_to_binary(Term, [{minor_version, 1}]).

foreach_in_queue(Fun, Queue) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            ok;
        {{value, Item}, Queue2} ->
            Fun(Item),
            foreach_in_queue(Fun, Queue2)
    end.
