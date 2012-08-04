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
%%%

-module(erlport_options).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-export([
    parse/1,
    timeout/1
    ]).


-type option() :: nouse_stdio
    | {packet, 1 | 2 | 4}
    | {python, Python :: string()}
    | {python_path, Path :: string() | [Path :: string()]}
    | {start_timeout, pos_integer() | infinity}
    | {call_timeout, pos_integer() | infinity}
    | {env, [{Name :: string(), Value :: string() | false}]}.
-type options() :: [option()].

-export_type([option/0, options/0]).

-include_lib("kernel/include/file.hrl").
-include("erlport.hrl").


%%
%% @doc Parse ErlPort options
%%

-spec parse(Options) -> Result when
    Options :: options(),
    Result :: {ok, #options{}} | {error, Reason},
    Reason :: term().

parse(Options) ->
    parse(Options, #options{}).

parse([nouse_stdio=UseStdio | Tail],
        Options=#options{port_options=PortOptions}) ->
    parse(Tail, Options#options{use_stdio=UseStdio,
        port_options=[UseStdio | PortOptions]});
parse([{packet, Packet}=Value | Tail], Options) ->
    case lists:member(Packet, [1, 2, 4]) of
        true ->
            parse(Tail, Options#options{packet=Packet});
        false ->
            {error, {invalid_option, Value}}
    end;
parse([{python, Python} | Tail], Options) ->
    % Will be checked later
    parse(Tail, Options#options{python=Python});
parse([{env, Env}=Value | Tail], Options) ->
    case filter_invalid_env(Env) of
        [] ->
            parse(Tail, Options#options{env=Env});
        Invalid ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse([{python_path, PythonPath}=Value | Tail], Options) ->
    case filter_invalid_paths(PythonPath) of
        {ok, Path} ->
            % Paths will be checked later
            parse(Tail, Options#options{python_path=Path});
        {error, Invalid} ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse([{start_timeout, Timeout}=Value | Tail], Options) ->
    case timeout(Timeout) of
        {ok, T} ->
            parse(Tail, Options#options{start_timeout=T});
        error ->
            {error, {invalid_option, Value}}
    end;
parse([{call_timeout, Timeout}=Value | Tail], Options) ->
    case timeout(Timeout) of
        {ok, T} ->
            parse(Tail, Options#options{call_timeout=T});
        error ->
            {error, {invalid_option, Value}}
    end;
parse([UnknownOption | _Tail], _Options) ->
    {error, {unknown_option, UnknownOption}};
parse([], Options=#options{env=Env0, python_path=PythonPath0, python=Python,
        port_options=PortOptions, packet=Packet}) ->
    case get_python(Python) of
        {ok, PythonFilename} ->
            case update_python_path(Env0, PythonPath0) of
                {ok, PythonPath, Env} ->
                    {ok, Options#options{env=Env, python_path=PythonPath,
                        python=PythonFilename,
                        port_options=[{env, Env}, {packet, Packet}
                            | PortOptions]}};
                {error, _}=Error ->
                    Error
            end;
        {error, _}=Error ->
            Error
    end.

%%%
%%% Utility functions
%%%

filter_invalid_paths(Paths=[List | _]) when is_list(List) ->
    case lists:filter(fun (L) -> not is_list(L) end, Paths) of
        [] ->
            {ok, Paths};
        Invalid ->
            {error, Invalid}
    end;
filter_invalid_paths(Path=[Integer | _]) when is_integer(Integer) ->
    case lists:filter(fun (I) -> not is_integer(I) end, Path) of
        "" ->
            {ok, string:tokens(Path, ":")};
        Invalid ->
            {error, Invalid}
    end;
filter_invalid_paths(List) when is_list(List) ->
    {error, invalid_path};
filter_invalid_paths(_Paths) ->
    {error, not_list}.

filter_invalid_env(Env) when is_list(Env) ->
    lists:filter(fun
        ({N, V}) when is_list(N), is_list(V) ->
            false;
        (_) ->
            true
        end, Env);
filter_invalid_env(_Env) ->
    not_list.

update_python_path(Env0, PythonPath0) ->
    case code:priv_dir(erlport) of
        {error, bad_name} ->
            {error, {not_found, "erlport/priv"}};
        PrivDir ->
            ErlPortPath = filename:join(PrivDir, "python"),
            {PathFromEnv, Env2} = case lists:keytake("PYTHONPATH", 1, Env0) of
                false ->
                    {"", Env0};
                {value, {"PYTHONPATH", P}, Env1} ->
                    {P, Env1}
            end,
            case join_python_path([[ErlPortPath], PythonPath0,
                    string:tokens(PathFromEnv, ":")]) of
                {ok, PythonPath} ->
                    Env3 = [{"PYTHONPATH", PythonPath} | Env2],
                    {ok, PythonPath, Env3};
                {error, _}=Error ->
                    Error
            end
    end.

join_python_path(Parts=[_|_]) ->
    remove_duplicate_path(lists:append(Parts), [], sets:new()).

remove_duplicate_path([P | Tail], Paths, Seen) ->
    case P of
        "" ->
            remove_duplicate_path(Tail, Paths, Seen);
        P ->
            case filelib:is_dir(P) of
                true ->
                    case sets:is_element(P, Seen) of
                        false ->
                            Seen2 = sets:add_element(P, Seen),
                            remove_duplicate_path(Tail, [P | Paths], Seen2);
                        true ->
                            remove_duplicate_path(Tail, Paths, Seen)
                    end;
                false ->
                    {error, {not_dir, P}}
            end
    end;
remove_duplicate_path([], Paths, _Seen) ->
    {ok, string:join(lists:reverse(Paths), ":")}.

get_python(Python=[_|_]) ->
    {PythonCommand, Options} = lists:splitwith(fun (C) ->
        C =/= $ 
        end, Python),
    case os:find_executable(PythonCommand) of
        false ->
            case Python of
                ?DEFAULT_PYTHON ->
                    {error, python_not_found};
                _ ->
                    {error, {invalid_option, {python, Python}, not_found}}
            end;
        Filename ->
            {ok, Filename ++ Options}
    end;
get_python(Python) ->
    {error, {invalid_option, {python, Python}}}.

timeout(Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, Timeout};
timeout(Timeout=infinity) ->
    {ok, Timeout};
timeout(_) ->
    error.
