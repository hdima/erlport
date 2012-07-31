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

-module(erlport_options_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlport.hrl").


parse_test_() ->
    fun () ->
        {ok, #options{python=Python, use_stdio=use_stdio,
            packet=4, python_path=PythonPath,
            env=Env, port_options=PortOptions}} = erlport_options:parse([]),
        ?assertEqual(match, re:run(Python, "/python$", [{capture, none}])),
        ?assertEqual(match, re:run(PythonPath, "/priv/python$",
            [{capture, none}])),
        ?assertEqual([{"PYTHONPATH", PythonPath}], Env),
        ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
            PortOptions)
    end.

use_stdio_option_test_() -> [
    ?_assertMatch({ok, #options{use_stdio=use_stdio}},
        erlport_options:parse([])),
    ?_assertMatch({ok, #options{use_stdio=nouse_stdio}},
        erlport_options:parse([nouse_stdio]))
    ].

packet_option_test_() -> [
    ?_assertMatch({ok, #options{packet=4}}, erlport_options:parse([])),
    ?_assertMatch({ok, #options{packet=4}},
        erlport_options:parse([{packet, 4}])),
    ?_assertMatch({ok, #options{packet=1}},
        erlport_options:parse([{packet, 1}])),
    ?_assertMatch({ok, #options{packet=2}},
        erlport_options:parse([{packet, 2}])),
    ?_assertEqual({error, {invalid_option, {packet, 3}}},
        erlport_options:parse([{packet, 3}]))
    ].

env_option_test_() -> [
    ?_assertMatch({ok, #options{env=[{"PYTHONPATH", PythonPath}],
        python_path=PythonPath}}, erlport_options:parse([])),
    ?_assertMatch({ok, #options{env=[{"PYTHONPATH", PythonPath},
        {"test", "true"}], python_path=PythonPath}},
        erlport_options:parse([{env, [{"test", "true"}]}])),
    ?_assertEqual({error, {invalid_option,
        {env, [{"test", "true"}, {test, "true"}, {"test", true}, invalid]},
            [{test, "true"}, {"test", true}, invalid]}},
        erlport_options:parse([{env, [{"test", "true"}, {test, "true"},
            {"test", true}, invalid]}])),
    ?_assertEqual({error, {invalid_option, {env, invalid_env}, not_list}},
        erlport_options:parse([{env, invalid_env}]))
    ].

python_option_test_() -> {setup,
    fun () ->
        TmpDir = erlport_test_utils:tmp_dir("erlport_options_tests"),
        BadName = filename:join(TmpDir, "not_executable"),
        ok = file:write_file(BadName, <<>>, [raw]),
        UnknownName = filename:join(TmpDir, "unknown"),
        GoodName = filename:join(TmpDir, "python"),
        ok = file:write_file(GoodName, <<>>, [raw]),
        ok = file:change_mode(GoodName, 8#00755),
        {TmpDir, GoodName, BadName, UnknownName}
    end,
    fun ({TmpDir, _, _, _}) ->
        ok = erlport_test_utils:remove_object(TmpDir)
    end,
    fun ({_, GoodName, BadName, UnknownName}) -> [
        fun () ->
            {ok, #options{python=Python}} = erlport_options:parse([]),
            ?assertEqual(match, re:run(Python, "/python$", [{capture, none}]))
        end,
        ?_assertMatch({ok, #options{python=GoodName}},
            erlport_options:parse([{python, GoodName}])),
        ?_assertEqual({error, {invalid_option, {python, BadName}, not_found}},
            erlport_options:parse([{python, BadName}])),
        ?_assertEqual({error, {invalid_option, {python, UnknownName},
                not_found}},
            erlport_options:parse([{python, UnknownName}])),
        ?_assertEqual({error, {invalid_option,
               {python, "erlport_tests_unknown_name"}, not_found}},
            erlport_options:parse([{python, "erlport_tests_unknown_name"}])),
        ?_assertEqual({error, {invalid_option, {python, not_string}}},
            erlport_options:parse([{python, not_string}]))
    ] end}.

python_path_option_test_() -> {setup,
    fun () ->
        TmpDir = erlport_test_utils:tmp_dir("erlport_options_tests"),
        TestPath1 = filename:join(TmpDir, "path1"),
        ok = file:make_dir(TestPath1),
        TestPath2 = filename:join(TmpDir, "path2"),
        ok = file:make_dir(TestPath2),
        UnknownPath = filename:join(TmpDir, "unknown"),
        {TmpDir, TestPath1, TestPath2, UnknownPath}
    end,
    fun ({TmpDir, _, _, _}) ->
        ok = erlport_test_utils:remove_object(TmpDir)
    end,
    fun ({_, TestPath1, TestPath2, UnknownPath}) -> [
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse([]),
            ?assertEqual(match, re:run(PythonPath, "/priv/python$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse(
                    [{python_path, [TestPath1]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse(
                    [{python_path, TestPath1}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse(
                    [{python_path, TestPath1 ++ ":" ++ TestPath2}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse(
                    [{python_path, [TestPath1]},
                    {env, [{"PYTHONPATH", TestPath2}]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = erlport_options:parse(
                    [{python_path, [TestPath1, TestPath2, ""]},
                    {env, [{"PYTHONPATH", TestPath2 ++ ":" ++ TestPath1}]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        ?_assertEqual({error, {not_dir, UnknownPath}},
            erlport_options:parse([{python_path, [TestPath1, UnknownPath]}])),
        ?_assertEqual({error, {invalid_option, {python_path, invalid_path},
                not_list}},
            erlport_options:parse([{python_path, invalid_path}])),
        ?_assertEqual({error, {invalid_option, {python_path, ""},
                invalid_path}},
            erlport_options:parse([{python_path, ""}]))
    ] end}.

unknown_option_test_() ->
    ?_assertEqual({error, {unknown_option, unknown}},
        erlport_options:parse([unknown])).
