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
%%% @doc ErlPort options handling
%%% @author Dmitry Vasiliev <dima@hlabs.org>
%%% @copyright 2009-2012 Dmitry Vasiliev <dima@hlabs.org>
%%% @private
%%%

-module(erlport_options).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    parse/1,
    timeout/1
    ]).


-type option() :: nouse_stdio
    | use_stdio
    | {compressed, 0..9}
    | {cd, Path :: string()}
    | {packet, 1 | 2 | 4}
    | {start_timeout, pos_integer() | infinity}
    | {call_timeout, pos_integer() | infinity}
    | {env, [{Name :: string(), Value :: string() | false}]}.
-type option_name() :: use_stdio
    | cd
    | compressed
    | packet
    | env
    | start_timeout
    | call_timeout.

-export_type([option/0]).


%%
%% @doc Parse generic ErlPort option
%%

-spec parse(Option::option()) ->
    {ok, option_name(), Value::term()} | {error, Reason::term()}.

parse(use_stdio=UseStdio) ->
    {ok, use_stdio, UseStdio};
parse(nouse_stdio=UseStdio) ->
    {ok, use_stdio, UseStdio};
parse({compressed, Level}=Value) ->
    if
        is_integer(Level) andalso Level >= 0 andalso Level =< 9 ->
            {ok, compressed, Level};
        true ->
            {error, {invalid_option, Value}}
    end;
parse({packet, Packet}=Value) ->
    case lists:member(Packet, [1, 2, 4]) of
        true ->
            {ok, packet, Packet};
        false ->
            {error, {invalid_option, Value}}
    end;
parse({cd, Path}=Value) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, cd, Path};
        false ->
            {error, {invalid_option, Value}}
    end;
parse({env, Env}=Value) ->
    case filter_invalid_env(Env) of
        [] ->
            {ok, env, Env};
        Invalid ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse({start_timeout, Timeout}=Value) ->
    case timeout(Timeout) of
        {ok, T} ->
            {ok, start_timeout, T};
        error ->
            {error, {invalid_option, Value}}
    end;
parse({call_timeout, Timeout}=Value) ->
    case timeout(Timeout) of
        {ok, T} ->
            {ok, call_timeout, T};
        error ->
            {error, {invalid_option, Value}}
    end;
parse(UnknownOption) ->
    {error, {unknown_option, UnknownOption}}.

%%%
%%% Utility functions
%%%

filter_invalid_env(Env) when is_list(Env) ->
    lists:filter(fun
        ({N, V}) when is_list(N), is_list(V) ->
            false;
        (_) ->
            true
        end, Env);
filter_invalid_env(_Env) ->
    not_list.

timeout(Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, Timeout};
timeout(Timeout=infinity) ->
    {ok, Timeout};
timeout(_) ->
    error.
