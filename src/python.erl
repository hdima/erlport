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

-module(python).

-behaviour(gen_server).

-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
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
    port :: port(),
    client = true :: boolean(),
    queue = queue:new() :: queue(),
    in_process :: {call, From::term(), Timer::term()} | {cast, Timer::term()}
    }).

-type state() :: #state{}.
-type option() :: client
    | nouse_stdio
    | {packet, 1 | 2 | 4}
    | {python, Python :: string()}
    | {python_path, Path :: string()}
    | {env, [{Name :: string(), Value :: string() | false}]}.
-type options() :: [option()].
-opaque instance() :: pid().

-export_type([option/0, options/0]).

-define(START_TIMEOUT, 15000).
-define(CALL_TIMEOUT, 15000).


start() ->
    start([]).

-spec start(Options) -> Result when
    Options :: options(),
    Result :: {ok, Instance} | {error, Reason},
    Instance :: instance(),
    Reason :: term().

start(Options) when is_list(Options) ->
    gen_server:start(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).


start_link() ->
    start_link([]).

start_link(Options) when is_list(Options) ->
    gen_server:start_link(?MODULE, Options, [{timeout, ?START_TIMEOUT}]).


-spec stop(Instance) -> ok when
    Instance :: instance().

stop(Instance) when is_pid(Instance) ->
    gen_server:cast(Instance, stop).


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


-spec cast(Instance, Module, Function, Args) -> ok when
    Instance :: instance(),
    Module :: atom(),
    Function :: atom(),
    Args :: list().

cast(Instance, Module, Function, Args) when is_pid(Instance),
        is_atom(Module), is_atom(Function), is_list(Args) ->
    gen_server:cast(Instance, {cast, Module, Function, Args}).


%%%
%%% Behaviour callbacks
%%%

-spec init(Options) -> Result when
    Options :: options(),
    Result :: {ok, state()} | {stop, Reason},
    Reason :: term().

init(Options) when is_list(Options) ->
    Stdio = stdio_option(Options),
    Packet = packet_option(Options),
    Python = python_option(Options),
    Client = client_option(Options),
    Env = env_option(Options),
    % TODO: Add custom args?
    % TODO: Default call timeout?
    Path = lists:concat([Python, " -u -m erlport.cli --packet=", Packet,
        " --", Stdio]),
    Port = open_port({spawn, Path},
        [{packet, Packet}, binary, Stdio, hide, {env, Env}]),
    {ok, #state{port=Port, client=Client}}.


handle_call({call, Module, Function, Args}, From, State=#state{port=Port,
        queue=Queue, in_process=InProcess, client=true})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'C', Module, Function, Args}),
    case InProcess of
        undefined ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {call, From, Timer},
                    {noreply, State#state{in_process=Info}};
                wait ->
                    Request = {call, From, Data},
                    Queue2 = queue:in(Request, Queue),
                    {noreply, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            Request = {call, From, Data},
            Queue2 = queue:in(Request, Queue),
            {noreply, State#state{queue=Queue2}}
    end;
handle_call({call, _, _, _}, _, State) ->
    {reply, {error, unable_to_call_in_server_mode}, State};
handle_call(Request, From, State) ->
    {reply, {error, {bad_request, ?MODULE, Request, From}}, State}.


handle_cast({cast, Module, Function, Args}, State=#state{port=Port,
        queue=Queue, in_process=InProcess, client=true})
        when is_atom(Module), is_atom(Function), is_list(Args) ->
    Data = term_to_binary({'M', Module, Function, Args}),
    case InProcess of
        undefined ->
            case try_to_send(Port, Data) of
                {ok, Timer} ->
                    Info = {cast, Timer},
                    {noreply, State#state{in_process=Info}};
                wait ->
                    Request = {cast, Data},
                    Queue2 = queue:in(Request, Queue),
                    {noreply, State#state{queue=Queue2}};
                fail ->
                    {stop, port_closed, State}
            end;
        _ ->
            Request = {cast, Data},
            Queue2 = queue:in(Request, Queue),
            {noreply, State#state{queue=Queue2}}
    end;
handle_cast({cast, _, _, _}, State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({Port, {data, Data}}, State=#state{client=true, port=Port,
        in_process=InProcess}) ->
    try binary_to_term(Data) of
        'R' ->
            case InProcess of
                undefined ->
                    {stop, orphan_result, State};
                {cast, Timer} ->
                    erlang:cancel_timer(Timer),
                    check_queue(State);
                {call, _From, _Timer} ->
                    {stop, unexpected_result, State}
            end;
        {'R', Result} ->
            case InProcess of
                undefined ->
                    {stop, orphan_result, State};
                {call, From, Timer} ->
                    erlang:cancel_timer(Timer),
                    gen_server:reply(From, Result),
                    check_queue(State);
                {cast, _Timer} ->
                    {stop, unexpected_result, State}
            end;
        _ ->
            % Ignore possible call requests
            {noreply, State}
    catch
        error:badarg ->
            case InProcess of
                {call, From, _Timer} ->
                    gen_server:reply(From, {error, invalid_result});
                _ ->
                    ok
            end,
            {stop, invalid_result, State}
    end;
handle_info({Port, {data, Data}}, State=#state{port=Port, queue=Queue}) ->
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


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


stdio_option(Options) ->
    case proplists:get_value(nouse_stdio, Options, false) of
        false ->
            use_stdio;
        _ ->
            nouse_stdio
    end.


packet_option(Options) ->
    case proplists:get_value(packet, Options, 4) of
        1 ->
            1;
        2 ->
            2;
        _ ->
            4
    end.


python_option(Options) ->
    case proplists:get_value(python, Options, "python") of
        Python=[_|_] ->
            Python;
        _ ->
            "python"
    end.

env_option(Options) ->
    Env = case proplists:get_value(env, Options) of
        E when is_list(E) ->
            E;
        _ ->
            []
    end,
    % FIXME: What about errors in code:priv_dir/1?
    ErlPortPath = filename:join(code:priv_dir(erlport), "python"),
    PathFromEnv = case proplists:get_value("PYTHONPATH", Env) of
        Path=[_|_] ->
            Path;
        _ ->
            ""
    end,
    Env2 = proplists:delete("PYTHONPATH", Env),
    PythonPath = case proplists:get_value(python_path, Options) of
        Path2=[_|_] ->
            join_python_path([ErlPortPath, Path2, PathFromEnv]);
        _ ->
            join_python_path([ErlPortPath, PathFromEnv])
    end,
    [{"PYTHONPATH", PythonPath} | Env2].


join_python_path(Parts=[_|_]) ->
    string:join([P || P=[_|_] <- Parts], ":").


client_option(Options) ->
    case proplists:get_value(client, Options, true) of
        false ->
            false;
        _ ->
            true
    end.

try_to_send(Port, Data) ->
    try_to_send(Port, Data, true).

try_to_send(Port, Data, NeedTimer) ->
    try port_command(Port, Data, [nosuspend]) of
        true ->
            case NeedTimer of
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
