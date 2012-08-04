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
    call/5,
    switch/4,
    switch/5
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
    timeout :: pos_integer() | infinity,
    port :: port(),
    % TODO: Rename to 'waiting'
    queue = queue:new() :: queue(),
    % TODO: Rename to 'sent' and convert to queue
    in_process :: {call | switch | switch_wait, From::term(), Timer::term()},
    call :: {Monitor::reference(), Timer::reference(), Pid::pid()}
    }).

-opaque instance() :: pid().

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

start(Options) ->
    start(start, Options).

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

start_link(Options) ->
    start(start_link, Options).

%%
%% @doc Stop Python instance
%%

-spec stop(Instance) -> ok when
    Instance :: instance().

stop(Instance) when is_pid(Instance) ->
    gen_fsm:send_all_state_event(Instance, stop).

%%
%% @equiv call(Instance, Module, Function, Args, [])
%%

-spec call(Instance, Module, Function, Args) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Result :: term().

call(Instance, Module, Function, Args) ->
    call(Instance, Module, Function, Args, []).

%%
%% @doc Call Python function with arguments and return result
%%

-spec call(Instance, Module, Function, Args, Options) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Options :: [Option],
    Option :: {timeout, Timeout},
    Timeout :: pos_integer() | infinity,
    Result :: term().

call(Instance, Module, Function, Args, Options) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args), is_list(Options) ->
    Request = {call, Module, Function, Args, Options},
    case gen_fsm:sync_send_event(Instance, Request, infinity) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            % TODO: Unpack Error if needed
            erlang:error(Error)
    end.

%%
%% @equiv switch(Instance, Module, Function, Args, [])
%%

-spec switch(Instance, Module, Function, Args) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Result :: ok | {error, Reason},
    Reason :: term().

switch(Instance, Module, Function, Args) ->
    switch(Instance, Module, Function, Args, []).

%%
%% @doc Pass control to Python by calling the function with arguments
%%

-spec switch(Instance, Module, Function, Args, Options) -> Result when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list(),
    Options :: [Option],
    Option :: {timeout, Timeout} | wait,
    Timeout :: pos_integer() | infinity,
    Result :: ok | term() | {error, Reason},
    Reason :: term().

switch(Instance, Module, Function, Args, Options) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args), is_list(Options) ->
    Request = {switch, Module, Function, Args, Options},
    case proplists:get_value(wait, Options, false) of
        false ->
            gen_fsm:sync_send_event(Instance, Request, infinity);
        _ ->
            case gen_fsm:sync_send_event(Instance, Request, infinity) of
                {ok, Result} ->
                    Result;
                {error, Error} ->
                    % TODO: Unpack Error if needed
                    erlang:error(Error)
            end
    end.


%%%
%%% Behaviour callbacks
%%%

%%
%% @doc Process initialization callback
%% @hidden
%%

-spec init(Options) -> Result when
    Options :: #options{},
    Result :: {ok, client, #state{}} | {stop, Reason},
    Reason :: term().

init(#options{python=Python,use_stdio=UseStdio, packet=Packet,
        port_options=PortOptions, call_timeout=Timeout}) ->
    Path = lists:concat([Python,
        % Binary STDIO
        " -u",
        " -m erlport.cli",
        " --packet=", Packet,
        " --", UseStdio]),
    try open_port({spawn, Path}, PortOptions) of
        Port ->
            {ok, client, #state{port=Port, timeout=Timeout}}
    catch
        error:Error ->
            {stop, {open_port_error, Error}}
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

client({call, Module, Function, Args, Options}, From, State=#state{
        timeout=DefaultTimeout}) when is_atom(Module), is_atom(Function),
        is_list(Args), is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = encode({'C', Module, Function, Args}),
            send_request(call, From, Data, client, State, Timeout);
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            {reply, Error, client, State}
    end;
client({switch, Module, Function, Args, Options}, From, State=#state{
        timeout=DefaultTimeout}) when is_atom(Module), is_atom(Function),
        is_list(Args), is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = encode({'S', Module, Function, Args}),
            case proplists:get_value(wait, Options, false) of
                false ->
                    send_request(switch, From, Data, server, State, Timeout);
                _ ->
                    send_request(switch_wait, From, Data, server, State,
                        Timeout)
            end;
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            {reply, Error, client, State}
    end;
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

client(Error=timeout, State) ->
    {stop, Error, State};
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
    case send_data(Port, Data) of
        ok ->
            {next_state, server, State#state{call=undefined}};
        error ->
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
            {stop, {invalid_response, Response}, State}
    catch
        error:badarg ->
            {stop, {invalid_response_data, Data}, State}
    end;
handle_info({Port, {data, Data}}, StateName=server, State=#state{port=Port,
        timeout=Timeout, in_process=InProcess}) ->
    try binary_to_term(Data) of
        's' ->
            case InProcess of
                {switch, From, Timer} ->
                    gen_fsm:cancel_timer(Timer),
                    gen_fsm:reply(From, ok),
                    {next_state, StateName, State#state{in_process=undefined}};
                {switch_wait, _From, Timer} ->
                    gen_fsm:cancel_timer(Timer),
                    {next_state, StateName, State};
                undefined ->
                    {stop, orphan_response, State};
                _ ->
                    {stop, unexpected_response, State}
            end;
        {'C', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            Timer = gen_fsm:send_event_after(Timeout, timeout),
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
        {'r', Result} ->
            case InProcess of
                {switch_wait, From, _} ->
                    gen_fsm:reply(From, {ok, Result}),
                    {next_state, client, State#state{in_process=undefined}};
                undefined ->
                    {next_state, client, State}
            end;
        {'e', Error} ->
            case InProcess of
                {switch_wait, From, _} ->
                    gen_fsm:reply(From, {error, Error}),
                    {next_state, client, State#state{in_process=undefined}};
                undefined ->
                    {stop, {switch_failed, Error}, State}
            end;
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
    case send_data(Port, encode(R)) of
        ok ->
            {next_state, StateName, State#state{call=undefined}};
        error ->
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

send_data(Port, Data) ->
    try port_command(Port, Data) of
        true ->
            ok
    catch
        error:badarg ->
            error
    end.

process_queue(StateName=client, State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State};
        {{value, Queued}, Queue2} ->
            send_from_queue(Queued, Queue2, StateName, State)
    end.

send_request(Type, From, Data, StateName, State=#state{port=Port,
        queue=Queue, in_process=InProcess}, Timeout) ->
    Info = {Type, From, gen_fsm:send_event_after(Timeout, timeout)},
    case InProcess of
        undefined ->
            case send_data(Port, Data) of
                ok ->
                    {next_state, StateName, State#state{in_process=Info}};
                error ->
                    {stop, port_closed, State}
            end;
        _ ->
            Queue2 = queue:in({Info, Data}, Queue),
            {next_state, StateName, State#state{queue=Queue2}}
    end.

send_from_queue({Info, Data}, Queue, StateName, State=#state{port=Port}) ->
    case send_data(Port, Data) of
        ok ->
            {next_state, StateName, State#state{in_process=Info, queue=Queue}};
        error ->
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

start(Function, OptionsList) when is_list(OptionsList) ->
    case erlport_options:parse(OptionsList) of
        {ok, Options=#options{start_timeout=Timeout}} ->
            gen_fsm:Function(?MODULE, Options, [{timeout, Timeout}]);
        Error={error, _} ->
            Error
    end.
