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

-behaviour(gen_server).

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
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-record(state, {
    port :: port(),
    client = true :: boolean(),
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
    gen_server:start(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).

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
    gen_server:start_link(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).

%%
%% @doc Stop Python instance
%%

-spec stop(Instance) -> ok when
    Instance :: instance().

stop(Instance) when is_pid(Instance) ->
    gen_server:cast(Instance, stop).

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
    case gen_server:call(Instance, {call, Module, Function, Args}) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            erlang:error(Error)
    end.

%%
%% @doc Call Python function with arguments without waiting for result
%%

-spec cast(Instance, Module, Function, Args) -> ok when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list().

cast(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_server:cast(Instance, {cast, Module, Function, Args}).

%%
%% @doc Pass control to Python by calling functions with arguments
%%

-spec switch(Instance, Module, Function, Args) -> ok when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list().

switch(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_server:cast(Instance, {switch, Module, Function, Args}).


%%%
%%% Behaviour callbacks
%%%

%% @hidden

-spec init(Options) -> Result when
    Options :: erlport_options:options(),
    Result :: {ok, #state{}} | {stop, Reason},
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
                    {ok, #state{port=Port}}
            catch
                error:Error ->
                    {stop, {open_port_error, Error}}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @hidden

handle_call({call, Module, Function, Args}, From, State=#state{client=true})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'C', Module, Function, Args}),
    try_to_request(State, Data, {call, From});
handle_call({call, _, _, _}, _, State) ->
    {reply, {error, server_mode}, State};
handle_call(Request, From, State) ->
    {reply, {error, {bad_request, ?MODULE, Request, From}}, State}.

%% @hidden

handle_cast({cast, Module, Function, Args}, State=#state{client=true})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'M', Module, Function, Args}),
    try_to_request(State, Data, cast);
handle_cast({cast, _, _, _}, State) ->
    {noreply, State};
handle_cast({switch, Module, Function, Args}, State=#state{client=true})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'S', Module, Function, Args}),
    try_to_request(State#state{client=false}, Data, switch);
handle_cast({switch, _, _, _}, State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden

handle_info({Port, {data, Data}}, State=#state{client=true, port=Port,
        in_process=InProcess}) ->
    try binary_to_term(Data) of
        'R' ->
            case InProcess of
                undefined ->
                    {stop, orphan_response, State};
                {cast, Timer} ->
                    erlang:cancel_timer(Timer),
                    check_queue(State);
                {call, _From, _Timer} ->
                    {stop, unexpected_response, State}
            end;
        {'R', Result} ->
            case InProcess of
                undefined ->
                    {stop, orphan_response, State};
                {call, From, Timer} ->
                    erlang:cancel_timer(Timer),
                    gen_server:reply(From, Result),
                    check_queue(State);
                {cast, _Timer} ->
                    {stop, unexpected_response, State}
            end;
        _ ->
            % Ignore possible call requests
            {noreply, State}
    catch
        error:badarg ->
            case InProcess of
                {call, From, _Timer} ->
                    gen_server:reply(From, {error, invalid_response});
                _ ->
                    ok
            end,
            {stop, invalid_response, State}
    end;
handle_info({Port, {data, Data}}, State=#state{port=Port, queue=Queue}) ->
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
            {noreply, State};
        {'M', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            proc_lib:spawn(fun () -> apply(Module, Function, Args) end),
            Data = term_to_binary('R'),
            case try_to_send(Port, Data, false) of
                ok ->
                    {noreply, State};
                wait ->
                    Queue2 = queue:in({response, Data}, Queue),
                    {noreply, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            {noreply, State}
    catch
        error:badarg ->
            {stop, invalid_request, State}
    end;
% TODO: More port related handlers
handle_info(timeout, State=#state{in_process=InProcess}) ->
    case InProcess of
        {call, From, _} ->
            gen_server:reply(From, {error, timeout}),
            {stop, timeout, State};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden

terminate(_Reason, _State) ->
    ok.

%% @hidden

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
                    Timer = erlang:send_after(?CALL_TIMEOUT, self(), timeout),
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

check_queue(State=#state{port=Port, queue=Queue, client=true}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {noreply, State#state{in_process=undefined}};
        {{value, {call, Client, Data}}, Queue2} ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {call, Client, Timer},
                    {noreply, State#state{in_process=Info, queue=Queue2}};
                wait ->
                    {noreply, State#state{in_process=undefined}};
                fail ->
                    {stop, port_closed, State}
            end;
        {{value, {cast, Data}}, Queue2} ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {cast, Timer},
                    {noreply, State#state{in_process=Info, queue=Queue2}};
                wait ->
                    {noreply, State#state{in_process=undefined}};
                fail ->
                    {stop, port_closed, State}
            end
    end;
check_queue(State=#state{port=Port, queue=Queue}) ->
    case queue:out(Queue) of
        {empty, Queue} ->
            {noreply, State#state{in_process=undefined}};
        {{value, {response, Data}}, Queue2} ->
            case try_to_send(Port, Data, false) of
                ok ->
                    {noreply, State#state{queue=Queue2}};
                wait ->
                    {noreply, State};
                fail ->
                    {stop, port_closed, State}
            end
    end.

try_to_request(State=#state{port=Port, queue=Queue, in_process=InProcess},
        Data, Type) ->
    case InProcess of
        undefined ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = request(Type, Timer),
                    {noreply, State#state{in_process=Info}};
                wait ->
                    Queue2 = queue:in(queued(Type, Data), Queue),
                    {noreply, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            Queue2 = queue:in(queued(Type, Data), Queue),
            {noreply, State#state{queue=Queue2}}
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
