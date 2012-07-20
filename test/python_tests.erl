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

-module(python_tests).

-include_lib("eunit/include/eunit.hrl").


setup() ->
    {ok, P} = python:start(),
    P.

cleanup(P) ->
    ok = python:stop(P).

call_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) -> [
        ?_assertEqual(4, python:call(P, operator, add, [2, 2]))
    ] end}.

call_queue_test_() -> {setup,
    fun setup/0,
    fun cleanup/1,
    fun (P) ->
        {inparallel, [
            ?_assertEqual(N + 1, python:call(P, operator, add, [N , 1]))
            || N <- lists:seq(1, 50)]}
    end}.

cast_test_() -> {setup,
    fun () ->
        P = setup(),
        TestFile = string:strip(?cmd("/bin/mktemp -t python_tests.XXXXXXXX"),
            right, $\n),
        {P, TestFile}
    end,
    fun ({_, TestFile}) ->
        ok = file:delete(TestFile)
    end,
    fun ({P, TestFile}) ->
        fun () ->
            ?assertEqual(ok, python:cast(P, os, system,
                [<<"echo 'OK' > ", (list_to_binary(TestFile))/binary>>])),
            timer:sleep(500),
            ?assertEqual({ok, <<"OK\n">>}, file:read_file(TestFile))
        end
    end}.

cast_queue_test_() -> {setup,
    fun () ->
        P = setup(),
        TestDir = string:strip(?cmd("/bin/mktemp -d -t python_tests.XXXXXXXX"),
            right, $\n),
        {P, TestDir}
    end,
    fun ({_, TestDir}) ->
        ?cmd("/bin/rm -rf " ++ TestDir)
    end,
    fun ({P, TestDir}) ->
        {inparallel, [
            fun () ->
                TestFile = filename:join(TestDir, integer_to_list(N)),
                ?assertEqual(ok, python:cast(P, os, system,
                        [<<"echo 'OK' > ",
                            (list_to_binary(TestFile))/binary>>])),
                timer:sleep(500),
                ?assertEqual({ok, <<"OK\n">>}, file:read_file(TestFile))
            end || N <- lists:seq(1, 50)]}
    end}.
