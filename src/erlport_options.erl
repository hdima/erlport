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

-module(erlport_options).

-export([
    parse/1
    ]).

-type option() :: server
    | nouse_stdio
    | {packet, 1 | 2 | 4}
    | {python, Python :: string()}
    | {python_path, Path :: string()}
    | {env, [{Name :: string(), Value :: string() | false}]}.
-type options() :: [option()].

-export_type([option/0, options/0]).

-include_lib("kernel/include/file.hrl").
-include("erlport.hrl").

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
parse([{python, Python}=Value | Tail], Options) ->
    Paths = string:tokens(os:getenv("PATH"), ":"),
    case file:path_open(Paths, Python, [read, raw]) of
        {ok, F, Filename} ->
            ok = file:close(F),
            case file:read_file_info(Filename) of
                {ok, #file_info{mode=Mode}} when Mode band 8#00100 =/= 0 ->
                    parse(Tail, Options#options{python=Filename});
                _ ->
                    {error, {invalid_option, Value, non_executable}}
            end;
        {error, Reason} ->
            {error, {invalid_option, Value, Reason}}
    end;
parse([server | Tail], Options) ->
    parse(Tail, Options#options{is_client_mode=false});
parse([{env, Env}=Value | Tail], Options) ->
    case filter_invalid_env(Env) of
        [] ->
            parse(Tail, Options#options{env=Env});
        Invalid ->
            {error, {invalid_option, Value, Invalid}}
    end;
parse([{python_path, PythonPath} | Tail], Options) ->
    % TODO: Check all dirs?
    parse(Tail, Options#options{python_path=PythonPath});
parse([UnknownOption | _Tail], _Options) ->
    {error, {unknown_option, UnknownOption}};
parse([], Options=#options{env=Env0, python_path=PythonPath,
        port_options=PortOptions, packet=Packet}) ->
    Env = set_python_path(Env0, PythonPath),
    {ok, Options#options{env=Env,
        port_options=[{env, Env}, {packet, Packet} | PortOptions]}}.

filter_invalid_env(Env) when is_list(Env) ->
    lists:filter(fun
        ({N, V}) when is_list(N), is_list(V) ->
            false;
        (_) ->
            true
        end, Env);
filter_invalid_env(_Env) ->
    non_list.

set_python_path(Env, PythonPath0) ->
    % FIXME: What about errors in code:priv_dir/1?
    ErlPortPath = filename:join(code:priv_dir(erlport), "python"),
    PathFromEnv = proplists:get_value("PYTHONPATH", Env),
    PythonPath = join_python_path([ErlPortPath, PythonPath0, PathFromEnv]),
    [{"PYTHONPATH", PythonPath} | proplists:delete("PYTHONPATH", Env)].

join_python_path(Parts=[_|_]) ->
    string:join([P || P=[_|_] <- Parts], ":").
