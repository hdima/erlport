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
    compressed = 0 :: 0..9,
    port :: port(),
    queue = queue:new() :: queue(),
    % {call | switch | swtich_wait, From::term(), Timer::reference()}
    sent = queue:new() :: queue(),
    call :: {Pid::pid(), Timer::reference()}
    }).

-record(python, {
    pid :: pid()
    }).

-opaque instance() :: #python{}.

-include("python.hrl").

%%
%% @equiv start([])
%%

-spec start() ->
    {ok, instance()} | {error, Reason::term()}.

start() ->
    start([]).

%%
%% @doc Start Python instance
%%

-spec start(Options::python_options:options()) ->
    {ok, instance()} | {error, Reason::term()}.

start(Options) ->
    start(start, Options).

%%
%% @equiv start_link([])
%%

-spec start_link() ->
    {ok, instance()} | {error, Reason::term()}.

start_link() ->
    start_link([]).

%%
%% @doc Start linked Python instance
%%

-spec start_link(Options::python_options:options()) ->
    {ok, instance()} | {error, Reason::term()}.

start_link(Options) ->
    start(start_link, Options).

%%
%% @doc Stop Python instance
%%

-spec stop(Instance::instance()) -> ok.

stop(#python{pid=Pid}) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%
%% @equiv call(Instance, Module, Function, Args, [])
%%

-spec call(Instance::instance(), Module::atom(), Function::atom(),
        Args::list()) ->
    Result::term().

call(Instance, Module, Function, Args) ->
    call(Instance, Module, Function, Args, []).

%%
%% @doc Call Python function with arguments and return result
%%

-spec call(Instance::instance(), Module::atom(), Function::atom(),
        Args::list(),
        Options::[{timeout, Timeout::pos_integer() | infinity}]) ->
    Result::term().

call(#python{pid=Pid}, Module, Function, Args, Options) when is_atom(Module),
        is_atom(Function), is_list(Args), is_list(Options) ->
    Request = {call, Module, Function, Args, Options},
    case gen_fsm:sync_send_event(Pid, Request, infinity) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            % TODO: Unpack Error if needed
            erlang:error(Error)
    end.

%%
%% @equiv switch(Instance, Module, Function, Args, [])
%%

-spec switch(Instance::instance(), Module::atom(), Function::atom(),
        Args::list()) ->
    Result::term().

switch(Instance, Module, Function, Args) ->
    switch(Instance, Module, Function, Args, []).

%%
%% @doc Pass control to Python by calling the function with arguments
%%

-spec switch(Instance::instance(), Module::atom(), Function::atom(),
        Args::list(),
        Options::[{timeout, Timeout::pos_integer() | infinity} | block]) ->
    Result::ok | term() | {error, Reason::term()}.

switch(#python{pid=Pid}, Module, Function, Args, Options) when is_atom(Module),
        is_atom(Function), is_list(Args), is_list(Options) ->
    Request = {switch, Module, Function, Args, Options},
    case proplists:get_value(block, Options, false) of
        false ->
            gen_fsm:sync_send_event(Pid, Request, infinity);
        _ ->
            case gen_fsm:sync_send_event(Pid, Request, infinity) of
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
init(#python_options{python=Python,use_stdio=UseStdio, packet=Packet,
        compressed=Compressed, port_options=PortOptions,
        call_timeout=Timeout}) ->
    Path = lists:concat([Python,
        % Binary STDIO
        " -u",
        " -m erlport.cli",
        " --packet=", Packet,
        " --", UseStdio,
        " --compressed=", Compressed]),
    try open_port({spawn, Path}, PortOptions) of
        Port ->
            process_flag(trap_exit, true),
            {ok, client, #state{port=Port, timeout=Timeout,
                compressed=Compressed}}
    catch
        error:Error ->
            {stop, {open_port_error, Error}}
    end.

%%
%% @doc Synchronous event handler in client mode
%% @hidden
%%
client({call, Module, Function, Args, Options}, From, State=#state{
        timeout=DefaultTimeout, compressed=Compressed})
        when is_atom(Module), is_atom(Function), is_list(Args),
        is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = erlport_utils:encode_term({'C', Module, Function,
                erlport_utils:prepare_list(Args)}, Compressed),
            send_request(call, From, Data, client, State, Timeout);
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            {reply, Error, client, State}
    end;
client({switch, Module, Function, Args, Options}, From, State=#state{
        timeout=DefaultTimeout, compressed=Compressed})
        when is_atom(Module), is_atom(Function), is_list(Args),
        is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = erlport_utils:encode_term({'S', Module, Function,
                erlport_utils:prepare_list(Args)}, Compressed),
            case proplists:get_value(block, Options, false) of
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
client(Error=timeout, State) ->
    {stop, Error, State};
client(_Event, State) ->
    {next_state, client, State}.

%%
%% @doc Synchronous event handler in server mode
%% @hidden
%%
server(_Event, _From, State) ->
    {reply, {error, server_mode}, server, State}.

%%
%% @doc Asynchronous event handler in server mode
%% @hidden
%%
server(timeout, State=#state{call={Pid, _Timer}}) ->
    true = exit(Pid, timeout),
    {next_state, server, State};
server(_Event, State) ->
    {next_state, server, State}.

%%
%% @doc Generic asynchronous event handler
%% @hidden
%%
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
        timeout=Timeout, sent=Sent}) ->
    try binary_to_term(Data) of
        {'C', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            Pid = proc_lib:spawn_link(fun () ->
                exit(try {ok, apply(Module, Function, Args)}
                    catch
                        Class:Reason ->
                            {error, {Class, Reason, erlang:get_stacktrace()}}
                    end)
                end),
            Info = {Pid, erlport_utils:start_timer(Timeout)},
            {next_state, StateName, State#state{call=Info}};
        's' ->
            case queue:out(Sent) of
                {{value, {switch, From, Timer}}, Sent2} ->
                    erlport_utils:stop_timer(Timer),
                    gen_fsm:reply(From, ok),
                    {next_state, StateName, State#state{sent=Sent2}};
                {{value, {switch_wait, _From, Timer}}, _Sent2} ->
                    erlport_utils:stop_timer(Timer),
                    {next_state, StateName, State};
                {empty, Sent} ->
                    {stop, orphan_response, State};
                _ ->
                    {stop, unexpected_response, State}
            end;
        {'r', Result} ->
            case queue:out(Sent) of
                {{value, {switch_wait, From, _}}, Sent2} ->
                    gen_fsm:reply(From, {ok, Result}),
                    {next_state, client, State#state{sent=Sent2}};
                {empty, Sent} ->
                    % switch(_wait) should be the last request in the queue
                    {next_state, client, State}
            end;
        {'e', Error} ->
            case queue:out(Sent) of
                {{value, {switch_wait, From, _}}, Sent2} ->
                    gen_fsm:reply(From, {error, Error}),
                    {next_state, client, State#state{sent=Sent2}};
                {empty, Sent} ->
                    % switch(_wait) should be the last request in the queue
                    {stop, {switch_failed, Error}, State}
            end;
        Request ->
            {stop, {invalid_request, Request}, State}
    catch
        error:badarg ->
            {stop, {invalid_request_data, Data}, State}
    end;
handle_info({'EXIT', Pid, Result}, StateName=server, State=#state{port=Port,
	call={Pid, Timer}, compressed=Compressed}) ->
    erlport_utils:stop_timer(Timer),
    R = case Result of
        {ok, Response} ->
            {'r', erlport_utils:prepare_term(Response)};
        {error, Response} ->
            {'e', erlport_utils:prepare_term(Response)};
        Response ->
            {'e', {error, erlport_utils:prepare_term(Response), []}}
    end,
    case erlport_utils:send_data(Port, erlport_utils:encode_term(R, Compressed)) of
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

terminate(Reason, _StateName, #state{sent=Sent, queue=Queue}) ->
    Error = case Reason of
        normal ->
            {error, stopped};
        Reason ->
            {error, Reason}
    end,
    queue_foreach(fun ({_Type, From, _Timer}) ->
        gen_fsm:reply(From, Error)
        end, Sent),
    queue_foreach(fun ({_Type, From, _Data}) ->
        gen_fsm:reply(From, Error)
        end, Queue).

%% @hidden

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%
%%% Internal functions
%%%

send_request(Type, From, Data, StateName, State=#state{port=Port,
        queue=Queue, sent=Sent}, Timeout) ->
    Info = {Type, From, erlport_utils:start_timer(Timeout)},
    case queue:is_empty(Sent) of
        true ->
            send_request(Info, Data, Queue, StateName, State);
        false ->
            case erlport_utils:try_send_data(Port, Data) of
                ok ->
                    Sent2 = queue:in(Info, Sent),
                    {next_state, StateName, State#state{sent=Sent2}};
                wait ->
                    Queue2 = queue:in({Info, Data}, Queue),
                    {next_state, StateName, State#state{queue=Queue2}};
                error ->
                    {stop, port_closed, State}
            end
    end.

send_request(Info, Data, Queue, StateName, State=#state{port=Port,
        sent=Sent}) ->
    case erlport_utils:send_data(Port, Data) of
        ok ->
            Sent2 = queue:in(Info, Sent),
            {next_state, StateName, State#state{sent=Sent2, queue=Queue}};
        error ->
            {stop, port_closed, State}
    end.

handle_response(ExpectedType, Response, State=#state{sent=Sent}, StateName) ->
    case queue:out(Sent) of
        {{value, {ExpectedType, From, Timer}}, Sent2} ->
            erlport_utils:stop_timer(Timer),
            gen_fsm:reply(From, Response),
            process_queue(StateName, State#state{sent=Sent2});
        {empty, Sent} ->
            {stop, orphan_response, State};
        _ ->
            {stop, unexpected_response, State}
    end.

process_queue(StateName=client, State=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {next_state, StateName, State};
        {{value, Queued}, Queue2} ->
            send_from_queue(Queued, Queue2, StateName, State)
    end.

send_from_queue({Info, Data}, Queue, StateName, State=#state{port=Port,
        sent=Sent}) ->
    case queue:is_empty(Sent) of
        true ->
            send_request(Info, Data, Queue, StateName, State);
        false ->
            case erlport_utils:try_send_data(Port, Data) of
                ok ->
                    Sent2 = queue:in(Info, Sent),
                    {next_state, StateName, State#state{sent=Sent2,
                        queue=Queue}};
                wait ->
                    {next_state, StateName, State};
                error ->
                    {stop, port_closed, State}
            end
    end.

queue_foreach(Fun, Queue) ->
    case queue:out(Queue) of
        {{value, Item}, Queue2} ->
            Fun(Item),
            queue_foreach(Fun, Queue2);
        {empty, Queue} ->
            ok
    end.

start(Function, OptionsList) when is_list(OptionsList) ->
    case python_options:parse(OptionsList) of
        {ok, Options=#python_options{start_timeout=Timeout}} ->
            case gen_fsm:Function(?MODULE, Options, [{timeout, Timeout}]) of
                {ok, Pid} ->
                    {ok, #python{pid=Pid}};
                {error, _}=Error ->
                    Error
            end;
        Error={error, _} ->
            Error
    end.
