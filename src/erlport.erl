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
%%% @doc ErlPort interface
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2012 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(erlport).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-behaviour(gen_server).

-export([
    stop/1,
    call/5
    ]).

%% Behaviour callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-include("erlport.hrl").

-type server_instance() :: pid()
    | atom()
    | {Name::atom(), Node::atom()}
    | {global, GlobalName::term()}
    | {via, Module::atom(), ViaName::term()}.

-type call_option() :: {timeout, pos_integer() | infinity}
    | async.
-type call_options() :: [call_option()].

-export_type([server_instance/0, call_options/0]).

%%
%% @doc Stop port protocol
%%

-spec stop(Instance::server_instance()) -> ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%
%% @doc Call remote function with arguments and options and return result
%%

-spec call(Instance::server_instance(), Module::atom(), Function::atom(),
    Args::list(), Options::call_options()) -> Result::term().

call(Pid, Module, Function, Args, Options) when is_atom(Module)
        andalso is_atom(Function) andalso is_list(Args)
        andalso is_list(Options) ->
    ok = check_call_options(Options),
    Request = {call, Module, Function, Args, Options},
    case proplists:get_value(async, Options, false) of
        false ->
            % TODO: Timeout?
            call(Pid, Request, infinity);
        _ ->
            gen_server:cast(Pid, Request)
    end.


%%%
%%% Behaviour callbacks
%%%

%%
%% @doc Process initialization callback
%% @hidden
%%
init(Fun) when is_function(Fun, 0) ->
    process_flag(trap_exit, true),
    Fun().

%%
%% @doc Synchronous event handler
%% @hidden
%%
handle_call({call, Module, Function, Args, Options}, From, State=#state{
        timeout=DefaultTimeout, compressed=Compressed}) when is_atom(Module),
        is_atom(Function), is_list(Args), is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = erlport_utils:encode_term({'C', Module, Function,
                erlport_utils:prepare_list(Args)}, Compressed),
            erlport_utils:try_send_request(call, From, Data, State, Timeout);
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            {reply, Error, State}
    end;
handle_call(Request, From, State) ->
    Error = {unknown_call, ?MODULE, Request, From},
    {reply, Error, State}.

%%
%% @doc Asynchronous event handler
%% @hidden
%%
handle_cast({call, Module, Function, Args, Options}, State=#state{
        timeout=DefaultTimeout, compressed=Compressed}) when is_atom(Module),
        is_atom(Function), is_list(Args), is_list(Options) ->
    % TODO: Duplicate code with handle_call()
    Timeout = proplists:get_value(timeout, Options, DefaultTimeout),
    case erlport_options:timeout(Timeout) of
        {ok, Timeout} ->
            Data = erlport_utils:encode_term({'C', Module, Function,
                erlport_utils:prepare_list(Args)}, Compressed),
            erlport_utils:try_send_request(call, unknown, Data, State, Timeout);
        error ->
            Error = {error, {invalid_option, {timeout, Timeout}}},
            {stop, Error, State}
    end;
% TODO: Need to check timeout handlers here
handle_cast(Error=timeout, State=#state{call={Pid, _Timer}}) ->
    true = exit(Pid, Error),
    {noreply, State};
handle_cast(Error=timeout, State) ->
    {stop, Error, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Event, State) ->
    {noreply, State}.

%%
%% @doc Messages handler
%% @hidden
%%
handle_info({Port, {data, Data}}, State=#state{port=Port, timeout=Timeout}) ->
    try binary_to_term(Data) of
        {'C', Module, Function, Args} when is_atom(Module), is_atom(Function),
                is_list(Args) ->
            Pid = spawn_call(Module, Function, Args),
            Info = {Pid, erlport_utils:start_timer(Timeout)},
            {noreply, State#state{call=Info}};
        {'r', Result} ->
            erlport_utils:handle_response(call, {ok, Result}, State);
        {'e', Error} ->
            erlport_utils:handle_response(call, {error, Error}, State);
        {'M', Pid, Message} ->
            send(Pid, Message, State);
        {'P', StdoutData} ->
            print(StdoutData, State);
        Request ->
            {stop, {invalid_protocol_message, Request}, State}
    catch
        error:badarg ->
            {stop, {invalid_protocol_data, Data}, State}
    end;
handle_info({'EXIT', Pid, Result}, State=#state{port=Port, call={Pid, Timer},
        compressed=Compressed}) ->
    erlport_utils:stop_timer(Timer),
    R = case Result of
        {ok, Response} ->
            {'r', erlport_utils:prepare_term(Response)};
        {error, Error} ->
            {'e', erlport_utils:prepare_term(Error)};
        Error ->
            {'e', {erlang, undefined, erlport_utils:prepare_term(Error), []}}
    end,
    case erlport_utils:send_data(Port,
            erlport_utils:encode_term(R, Compressed)) of
        ok ->
            {noreply, State#state{call=undefined}};
        error ->
            {stop, port_closed, State#state{call=undefined}}
    end;
handle_info({Port, closed}, State=#state{port=Port}) ->
    {stop, port_closed, State};
handle_info({'EXIT', Port, Reason}, State=#state{port=Port}) ->
    {stop, {port_closed, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%
%% @doc Server is about to terminate
%% @hidden
%%
terminate(Reason, #state{sent=Sent, queue=Queue}) ->
    Error = case Reason of
        normal ->
            {error, stopped};
        Reason ->
            {error, Reason}
    end,
    queue_foreach(fun ({_Type, From, _Timer}) ->
        case From of
            unknown ->
                ok;
            _ ->
                gen_server:reply(From, Error)
        end
        end, Sent),
    queue_foreach(fun ({{_Type, From, _Timer}, _Data}) ->
        case From of
            unknown ->
                ok;
            _ ->
                gen_server:reply(From, Error)
        end
        end, Queue).

%%
%% @doc Code change handler
%% @hidden
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

queue_foreach(Fun, Queue) ->
    case queue:out(Queue) of
        {{value, Item}, Queue2} ->
            Fun(Item),
            queue_foreach(Fun, Queue2);
        {empty, Queue} ->
            ok
    end.

call(Pid, Request, Timeout) ->
    case gen_server:call(Pid, Request, Timeout) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            erlang:error(Error)
    end.

print(Data, State) ->
    ok = io:put_chars(Data),
    {noreply, State}.

spawn_call(Module, Function, Args) ->
    proc_lib:spawn_link(fun () ->
        exit(try {ok, apply(Module, Function, Args)}
            catch
                error:{Language, Type, _Val, Trace}=Error
                        when is_atom(Language) andalso is_atom(Type)
                        andalso is_list(Trace) ->
                    {error, Error};
                Type:Reason ->
                    Trace = erlang:get_stacktrace(),
                    {error, {erlang, Type, Reason, Trace}}
            end)
        end).

check_call_options([{timeout, Timeout}=Value | Tail]) ->
    case erlport_options:timeout(Timeout) of
        {ok, _} ->
            check_call_options(Tail);
        error ->
            erlang:error({erlport_option_error, Value})
    end;
check_call_options([async | Tail]) ->
    check_call_options(Tail);
check_call_options([Invalid | _]) ->
    erlang:error({erlport_invalid_option, Invalid});
check_call_options([]) ->
    ok.

send(Pid, Message, State) ->
    % Ignore errors
    catch Pid ! Message,
    {noreply, State}.
