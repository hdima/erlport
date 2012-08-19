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

-module(python_options_tests).

-include_lib("eunit/include/eunit.hrl").
-include("python.hrl").


parse_test_() ->
    fun () ->
        {ok, #python_options{python=Python, use_stdio=use_stdio,
            call_timeout=10000, packet=4, python_path=PythonPath,
            start_timeout=10000, compressed=0, env=Env,
            port_options=PortOptions}} = python_options:parse([]),
        ?assertEqual(match, re:run(Python, "/python$", [{capture, none}])),
        ?assertEqual(match, re:run(PythonPath, "/priv/python$",
            [{capture, none}])),
        ?assertEqual([{"PYTHONPATH", PythonPath}], Env),
        ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
            PortOptions)
    end.

use_stdio_option_test_() -> [
    ?_assertMatch({ok, #python_options{use_stdio=use_stdio}},
        python_options:parse([])),
    ?_assertMatch({ok, #python_options{use_stdio=nouse_stdio}},
        python_options:parse([nouse_stdio]))
    ].

compressed_option_test_() -> [
    ?_assertMatch({ok, #python_options{compressed=0}},
        python_options:parse([])),
    ?_assertMatch({ok, #python_options{compressed=9}},
        python_options:parse([{compressed, 9}])),
    ?_assertMatch({error, {invalid_option, {compressed, invalid}}},
        python_options:parse([{compressed, invalid}]))
    ].

packet_option_test_() -> [
    ?_assertMatch({ok, #python_options{packet=4}}, python_options:parse([])),
    ?_assertMatch({ok, #python_options{packet=4}},
        python_options:parse([{packet, 4}])),
    ?_assertMatch({ok, #python_options{packet=1}},
        python_options:parse([{packet, 1}])),
    ?_assertMatch({ok, #python_options{packet=2}},
        python_options:parse([{packet, 2}])),
    ?_assertEqual({error, {invalid_option, {packet, 3}}},
        python_options:parse([{packet, 3}]))
    ].

start_timeout_test_() -> [
    ?_assertMatch({ok, #python_options{start_timeout=10000}},
        python_options:parse([])),
    ?_assertMatch({ok, #python_options{start_timeout=5000}},
        python_options:parse([{start_timeout, 5000}])),
    ?_assertMatch({ok, #python_options{start_timeout=infinity}},
        python_options:parse([{start_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, 0}}},
        python_options:parse([{start_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, invalid}}},
        python_options:parse([{start_timeout, invalid}]))
    ].

call_timeout_test_() -> [
    ?_assertMatch({ok, #python_options{call_timeout=10000}},
        python_options:parse([])),
    ?_assertMatch({ok, #python_options{call_timeout=5000}},
        python_options:parse([{call_timeout, 5000}])),
    ?_assertMatch({ok, #python_options{call_timeout=infinity}},
        python_options:parse([{call_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, 0}}},
        python_options:parse([{call_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, invalid}}},
        python_options:parse([{call_timeout, invalid}]))
    ].

env_option_test_() -> [
    ?_assertMatch({ok, #python_options{env=[{"PYTHONPATH", PythonPath}],
        python_path=PythonPath}}, python_options:parse([])),
    ?_assertMatch({ok, #python_options{env=[{"PYTHONPATH", PythonPath},
        {"test", "true"}], python_path=PythonPath}},
        python_options:parse([{env, [{"test", "true"}]}])),
    ?_assertEqual({error, {invalid_option,
        {env, [{"test", "true"}, {test, "true"}, {"test", true}, invalid]},
            [{test, "true"}, {"test", true}, invalid]}},
        python_options:parse([{env, [{"test", "true"}, {test, "true"},
            {"test", true}, invalid]}])),
    ?_assertEqual({error, {invalid_option, {env, invalid_env}, not_list}},
        python_options:parse([{env, invalid_env}]))
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
            {ok, #python_options{python=Python}} = python_options:parse([]),
            ?assertEqual(match, re:run(Python, "/python$", [{capture, none}]))
        end,
        ?_assertMatch({ok, #python_options{python=GoodName}},
            python_options:parse([{python, GoodName}])),
        fun () ->
            CommandWithOption = GoodName ++ " -S",
            ?assertMatch({ok, #python_options{python=CommandWithOption}},
                python_options:parse([{python, CommandWithOption}]))
        end,
        ?_assertEqual({error, {invalid_option, {python, BadName}, not_found}},
            python_options:parse([{python, BadName}])),
        ?_assertEqual({error, {invalid_option, {python, UnknownName},
                not_found}},
            python_options:parse([{python, UnknownName}])),
        ?_assertEqual({error, {invalid_option,
               {python, "erlport_tests_unknown_name"}, not_found}},
            python_options:parse([{python, "erlport_tests_unknown_name"}])),
        ?_assertEqual({error, {invalid_option, {python, not_string}}},
            python_options:parse([{python, not_string}])),
        fun () ->
            Path = os:getenv("PATH"),
            true = os:putenv("PATH", ""),
            try ?assertEqual({error, python_not_found},
                    python_options:parse([]))
            after
                true = os:putenv("PATH", Path)
            end
        end
    ] end}.

cd_option_test_() -> {setup,
    fun () ->
        erlport_test_utils:tmp_dir("erlport_options_tests")
    end,
    fun erlport_test_utils:remove_object/1,
    fun (TmpDir) -> [
        fun () ->
            {ok, #python_options{cd=undefined, port_options=PortOptions,
                env=Env}} = python_options:parse([]),
            ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
                PortOptions)
        end,
        fun () ->
            {ok, #python_options{cd=TmpDir, port_options=PortOptions, env=Env}}
                = python_options:parse([{cd, TmpDir}]),
            ?assertEqual([{env, Env}, {packet, 4}, {cd, TmpDir},
                binary, hide, exit_status], PortOptions)
        end,
        ?_assertEqual({error, {invalid_option, {cd, "invalid_directory"}}},
            python_options:parse([{cd, "invalid_directory"}]))
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
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse([]),
            ?assertEqual(match, re:run(PythonPath, "/priv/python$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{python_path, [TestPath1]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{python_path, TestPath1}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{python_path, TestPath1 ++ ":" ++ TestPath2}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{python_path, [TestPath1]},
                    {env, [{"PYTHONPATH", TestPath2}]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{env, [{"PYTHONPATH", TestPath1},
                    {"PYTHONPATH", TestPath2}]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #python_options{python_path=PythonPath,
                env=[{"PYTHONPATH", PythonPath}]=Env,
                port_options=[{env, Env} | _]}} = python_options:parse(
                    [{python_path, [TestPath1, TestPath2, ""]},
                    {env, [{"PYTHONPATH", TestPath2 ++ ":" ++ TestPath1}]}]),
            ?assertEqual(match, re:run(PythonPath,
                "/priv/python:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        ?_assertEqual({error, {not_dir, UnknownPath}},
            python_options:parse([{python_path, [TestPath1, UnknownPath]}])),
        ?_assertEqual({error, {invalid_option, {python_path, invalid_path},
                not_list}},
            python_options:parse([{python_path, invalid_path}])),
        ?_assertEqual({error, {invalid_option, {python_path, ""},
                invalid_path}},
            python_options:parse([{python_path, ""}])),
        ?_assertEqual({error, {invalid_option, {python_path,
                [TestPath1, invalid]}, [invalid]}},
            python_options:parse([{python_path, [TestPath1, invalid]}])),
        ?_assertEqual({error, {invalid_option, {python_path,
                [$a, $b, invalid]}, [invalid]}},
            python_options:parse([{python_path, [$a, $b, invalid]}])),
        fun () ->
            Dir = code:lib_dir(erlport),
            true = code:del_path(erlport),
            try ?assertEqual({error, {not_found, "erlport/priv"}},
                    python_options:parse([]))
            after
                true = code:add_patha(Dir)
            end
        end
    ] end}.

unknown_option_test_() ->
    ?_assertEqual({error, {unknown_option, unknown}},
        python_options:parse([unknown])).
