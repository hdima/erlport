%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
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
%%% @doc ErlPort test utils
%%%

-module(erlport_test_utils).

-export([
    tmp_file/1,
    tmp_dir/1,
    remove_object/1,
    script/1,
    call_with_env/3,
    match_path/2,
    local_path/1,
    create_mock_script/3,
    assert_output/3,
    del_code_path/2
    ]).

-include_lib("eunit/include/eunit.hrl").

-define(CHARS, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
    $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s,
    $t, $u, $v, $w, $x, $y, $z,
    $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S,
    $T, $U, $V, $W, $X, $Y, $Z}).
-define(TIMEOUT, 5000).

%%
%% @doc Create temporary file and return the file name
%%

-spec tmp_file(BaseName::string()) -> FileName::string().

tmp_file(BaseName) when is_list(BaseName) ->
    % Only needed for Erlang R13
    crypto:start(),
    tmp_file(BaseName, 3).

tmp_file(_BaseName, 0) ->
    erlang:error(unable_to_create);
tmp_file(BaseName, N) ->
    FileName = tmp_name(BaseName),
    case file:write_file(FileName, <<>>, [exclusive, raw]) of
        ok ->
            FileName;
        {error, eexist} ->
            tmp_file(BaseName, N - 1);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%
%% @doc Create temporary directory and return the directory name
%%

-spec tmp_dir(BaseName::string()) -> DirName::string().

tmp_dir(BaseName) when is_list(BaseName) ->
    % Only needed for Erlang R13
    crypto:start(),
    tmp_dir(BaseName, 3).

tmp_dir(_BaseName, 0) ->
    erlang:error(unable_to_create);
tmp_dir(BaseName, N) ->
    DirName = tmp_name(BaseName),
    case file:make_dir(DirName) of
        ok ->
            DirName;
        {error, enoent} ->
            tmp_dir(BaseName, N - 1);
        {error, enotdir} ->
            tmp_dir(BaseName, N - 1);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%
%% @doc Remove directory/file recursive
%%

-spec remove_object(DirName::string()) -> ok.

remove_object(DirName=[_|_]) ->
    case filelib:is_regular(DirName) of
        true ->
            ok = file:delete(DirName);
        false ->
            {ok, FileNames} = file:list_dir(DirName),
            lists:foreach(fun (FileName) ->
                remove_object(filename:join(DirName, FileName))
                end, FileNames),
            ok = file:del_dir(DirName)
    end.

%%
%% @doc Return script name depending on the current OS
%%

-spec script(Script::string()) -> ScriptName::string().

script(Script) ->
    case os:type() of
        {win32, _} ->
            local_path(Script) ++ ".bat";
        _ ->
            Script
    end.

%%
%% @doc Call the function with replaced environment variable value
%%

-spec call_with_env(Fun::fun(() -> term()), Key::string(), Value::string()) ->
    ok.

call_with_env(Fun, Key, Value) ->
    OldValue = os:getenv(Key),
    true = os:putenv(Key, Value),
    try Fun()
    after
        V = case OldValue of
            false ->
                "";
            OldValue ->
                OldValue
        end,
        true = os:putenv(Key, V),
        ok
    end.

%%
%% @doc Match the pattern against the path
%%

-spec match_path(Path::string(), Pattern::string() | [string()]) ->
    match | {nomatch, Path::string()}.

match_path(Path, Pattern) ->
    case re:run(Path, local_path(Pattern, "\\\\"), [{capture, none}]) of
        match ->
            match;
        _ ->
            {nomatch, Path}
    end.

%%
%% @doc Update the path depending on the current OS
%%

-spec local_path(Path::string() | [string()]) -> LocalPath::string().

local_path(Path) ->
    local_path(Path, "\\").

local_path([Part | _]=Paths, WinPathSep) when is_list(Part) ->
    Separator = case os:type() of
        {win32, _} ->
            ";";
        _ ->
            ":"
    end,
    string:join([local_path(P, WinPathSep) || P <- Paths], Separator);
local_path(Path, WinPathSep) ->
    case os:type() of
        {win32, _} ->
            str_replace(Path, $/, WinPathSep);
        _ ->
            Path
    end.

%%
%% @doc Create mock script which prints the Version string
%%

-spec create_mock_script(Version::string(), Dir::string(), Name::string()) ->
    Path::string().

create_mock_script(Version, Dir, Name) ->
    Path = filename:join(Dir, Name),
    write_script_file(Path, Version, os:type()),
    Path.

%%
%% @doc Assert function output
%%

-spec assert_output(Expected::binary(), Fun::fun(() -> term()),
    Printer::pid()) -> ok.

assert_output(Expected, Fun, Printer) when is_binary(Expected)
        andalso is_function(Fun, 0) andalso is_pid(Printer) ->
    OldLeader = group_leader(),
    Client = self(),
    Leader = spawn_link(fun () -> io_proxy(OldLeader, Printer, Client, []) end),
    true = group_leader(Leader, Printer),
    try Fun()
    after
        Leader ! stop,
        true = group_leader(OldLeader, Printer)
    end,
    assert_expected_output(Expected).

%%
%% @doc Delete directory from the code path
%% Directory can be added to the path more than one time
%%

-spec del_code_path(Path::atom(), N::pos_integer()) ->
    ok | {error, unable_to_del_path}.

del_code_path(_Path, 0) ->
    {error, unable_to_del_path};
del_code_path(Path, N) when N > 0 ->
    case code:del_path(Path) of
        true ->
            del_code_path(Path, N - 1);
        false ->
            ok
    end.

%%
%% Internal functions
%%

write_script_file(Path, Version, {win32, _}) ->
    BatPath = Path ++ ".bat",
    ok = file:write_file(BatPath,
        <<"@echo ", (list_to_binary(Version))/binary, "\n">>, [raw]);
write_script_file(Path, Version, _) ->
    ok = file:write_file(Path,
        <<"#! /bin/sh\n",
            "echo '", (list_to_binary(Version))/binary, "'\n">>, [raw]),
    ok = file:change_mode(Path, 8#00755).

tmp_name("") ->
    tmp_name("test");
tmp_name(BaseName=[_|_]) ->
    filename:join(get_base_dir(), BaseName ++ "." ++ random_name(8)).

random_name(0) ->
    [];
random_name(N) ->
    Chars = ?CHARS,
    I = crypto:rand_uniform(1, tuple_size(Chars) + 1),
    [element(I, Chars) | random_name(N - 1)].

get_base_dir() ->
    {ok, Path} = file:get_cwd(),
    Path.

str_replace(Str, Find, Replace) ->
    lists:flatten(lists:map(fun
        (C) ->
            case C of
                Find ->
                    Replace;
                C ->
                    C
            end
        end, Str)).

assert_expected_output(Expected) ->
    receive
        {output, Output} ->
            ?assertEqual(Expected, Output)
    after
        ?TIMEOUT ->
            erlang:error(timeout)
    end.

io_proxy(OldLeader, Printer, Client, Output) ->
    receive
        stop ->
            Client ! {output, iolist_to_binary(lists:reverse(Output))};
        Message ->
            OldLeader ! Message,
            io_proxy(OldLeader, Printer, Client,
                update_output(Message, Printer, Output))
    after
        ?TIMEOUT ->
            erlang:error(timeout)
    end.

update_output({io_request, Printer, _ReplyAs, Request}, Printer, Output) ->
    case Request of
        {put_chars, _Encoding, Chars} ->
            [Chars | Output];
        {put_chars, _Encoding, Module, Function, Args} ->
           [apply(Module, Function, Args) | Output];
        _ ->
            Output
    end;
update_output(_, _Printer, Output) ->
    Output.
