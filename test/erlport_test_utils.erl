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
%%% @doc ErlPort test utils
%%%

-module(erlport_test_utils).

-export([
    tmp_file/1,
    tmp_dir/1,
    remove_object/1
    ]).

-define(CHARS, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
    $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s,
    $t, $u, $v, $w, $x, $y, $z,
    $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S,
    $T, $U, $V, $W, $X, $Y, $Z}).


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

tmp_name("") ->
    tmp_name("test");
tmp_name(BaseName=[_|_]) ->
    filename:join(get_tmp_dir(), BaseName ++ "." ++ random_name(8)).

random_name(0) ->
    [];
random_name(N) ->
    Chars = ?CHARS,
    I = crypto:rand_uniform(1, tuple_size(Chars) + 1),
    [element(I, Chars) | random_name(N - 1)].

get_tmp_dir() ->
    case os:type() of
        {win32, _} ->
            "C://Temp";
        _ ->
            "/tmp"
    end.
