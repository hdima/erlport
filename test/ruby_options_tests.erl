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

-module(ruby_options_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ruby.hrl").


parse_test_() ->
    fun () ->
        {ok, #ruby_options{ruby=Ruby, use_stdio=use_stdio,
            call_timeout=infinity, packet=4, ruby_lib=RubyLib,
            start_timeout=10000, compressed=0, env=Env,
            port_options=PortOptions}} = ruby_options:parse([]),
        ?assertEqual(match, re:run(Ruby, "/ruby$", [{capture, none}])),
        ?assertEqual(match, re:run(RubyLib, "/priv/ruby$",
            [{capture, none}])),
        ?assertEqual([{"RUBYLIB", RubyLib}], Env),
        ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
            PortOptions)
    end.

use_stdio_option_test_() -> [
    ?_assertMatch({ok, #ruby_options{use_stdio=use_stdio}},
        ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{use_stdio=nouse_stdio}},
        ruby_options:parse([nouse_stdio])),
    ?_assertMatch({ok, #ruby_options{use_stdio=use_stdio}},
        ruby_options:parse([use_stdio]))
    ].

compressed_option_test_() -> [
    ?_assertMatch({ok, #ruby_options{compressed=0}},
        ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{compressed=9}},
        ruby_options:parse([{compressed, 9}])),
    ?_assertMatch({error, {invalid_option, {compressed, invalid}}},
        ruby_options:parse([{compressed, invalid}]))
    ].

packet_option_test_() -> [
    ?_assertMatch({ok, #ruby_options{packet=4}}, ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{packet=4}},
        ruby_options:parse([{packet, 4}])),
    ?_assertMatch({ok, #ruby_options{packet=1}},
        ruby_options:parse([{packet, 1}])),
    ?_assertMatch({ok, #ruby_options{packet=2}},
        ruby_options:parse([{packet, 2}])),
    ?_assertEqual({error, {invalid_option, {packet, 3}}},
        ruby_options:parse([{packet, 3}]))
    ].

start_timeout_test_() -> [
    ?_assertMatch({ok, #ruby_options{start_timeout=10000}},
        ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{start_timeout=5000}},
        ruby_options:parse([{start_timeout, 5000}])),
    ?_assertMatch({ok, #ruby_options{start_timeout=infinity}},
        ruby_options:parse([{start_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, 0}}},
        ruby_options:parse([{start_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {start_timeout, invalid}}},
        ruby_options:parse([{start_timeout, invalid}]))
    ].

call_timeout_test_() -> [
    ?_assertMatch({ok, #ruby_options{call_timeout=infinity}},
        ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{call_timeout=5000}},
        ruby_options:parse([{call_timeout, 5000}])),
    ?_assertMatch({ok, #ruby_options{call_timeout=infinity}},
        ruby_options:parse([{call_timeout, infinity}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, 0}}},
        ruby_options:parse([{call_timeout, 0}])),
    ?_assertEqual({error, {invalid_option, {call_timeout, invalid}}},
        ruby_options:parse([{call_timeout, invalid}]))
    ].

env_option_test_() -> [
    ?_assertMatch({ok, #ruby_options{env=[{"RUBYLIB", RubyLib}],
        ruby_lib=RubyLib}}, ruby_options:parse([])),
    ?_assertMatch({ok, #ruby_options{env=[{"RUBYLIB", RubyLib},
        {"test", "true"}], ruby_lib=RubyLib}},
        ruby_options:parse([{env, [{"test", "true"}]}])),
    ?_assertEqual({error, {invalid_option,
        {env, [{"test", "true"}, {test, "true"}, {"test", true}, invalid]},
            [{test, "true"}, {"test", true}, invalid]}},
        ruby_options:parse([{env, [{"test", "true"}, {test, "true"},
            {"test", true}, invalid]}])),
    ?_assertEqual({error, {invalid_option, {env, invalid_env}, not_list}},
        ruby_options:parse([{env, invalid_env}]))
    ].

ruby_option_test_() -> {setup,
    fun () ->
        TmpDir = erlport_test_utils:tmp_dir("erlport_options_tests"),
        BadName = filename:join(TmpDir, "not_executable"),
        ok = file:write_file(BadName, <<>>, [raw]),
        UnknownName = filename:join(TmpDir, "unknown"),
        GoodRuby = create_mock_ruby(TmpDir, "ruby", "1.8.7"),
        UnsupportedRuby = create_mock_ruby(TmpDir, "unsupported", "1.7.0"),
        UnsupportedRuby2 = create_mock_ruby(TmpDir, "unsupported2", "1.9.0"),
        InvalidRuby = create_mock_ruby(TmpDir, "invalid", "INVALID"),
        {TmpDir, GoodRuby, BadName, UnknownName,
            UnsupportedRuby, UnsupportedRuby2, InvalidRuby}
    end,
    fun (Info) ->
        ok = erlport_test_utils:remove_object(element(1, Info)) % TmpDir
    end,
    fun ({_, GoodRuby, BadName, UnknownName, UnsupportedRuby,
            UnsupportedRuby2, InvalidRuby}) -> [
        fun () ->
            {ok, #ruby_options{ruby=Ruby}} = ruby_options:parse([]),
            ?assertEqual(match, re:run(Ruby, "/ruby$", [{capture, none}]))
        end,
        ?_assertMatch({ok, #ruby_options{ruby=GoodRuby}},
            ruby_options:parse([{ruby, GoodRuby}])),
        fun () ->
            {ok, #ruby_options{ruby=GoodRuby, ruby_lib=RubyPath}}
                = ruby_options:parse([{ruby, GoodRuby}]),
            ?assertEqual(match, re:run(RubyPath, "/priv/ruby$",
                [{capture, none}]))
        end,
        fun () ->
            CommandWithOption = GoodRuby ++ " -S",
            ?assertMatch({ok, #ruby_options{ruby=CommandWithOption}},
                ruby_options:parse([{ruby, CommandWithOption}]))
        end,
        ?_assertEqual({error, {invalid_option, {ruby, BadName}, not_found}},
            ruby_options:parse([{ruby, BadName}])),
        ?_assertEqual({error, {invalid_option, {ruby, UnknownName},
                not_found}},
            ruby_options:parse([{ruby, UnknownName}])),
        ?_assertEqual({error, {invalid_option,
               {ruby, "erlport_tests_unknown_name"}, not_found}},
            ruby_options:parse([{ruby, "erlport_tests_unknown_name"}])),
        ?_assertEqual({error, {invalid_option, {ruby, not_string}}},
            ruby_options:parse([{ruby, not_string}])),
        fun () ->
            Path = os:getenv("PATH"),
            true = os:putenv("PATH", ""),
            try ?assertEqual({error, ruby_not_found},
                    ruby_options:parse([]))
            after
                true = os:putenv("PATH", Path)
            end
        end,
        ?_assertEqual({error, {unsupported_ruby_version, "ruby 1.7.0 \n"}},
            ruby_options:parse([{ruby, UnsupportedRuby}])),
        ?_assertEqual({error, {unsupported_ruby_version, "ruby 1.9.0 \n"}},
            ruby_options:parse([{ruby, UnsupportedRuby2}])),
        ?_assertEqual({error, {invalid_ruby, InvalidRuby}},
            ruby_options:parse([{ruby, InvalidRuby}]))
    ] end}.

cd_option_test_() -> {setup,
    fun () ->
        erlport_test_utils:tmp_dir("erlport_options_tests")
    end,
    fun erlport_test_utils:remove_object/1,
    fun (TmpDir) -> [
        fun () ->
            {ok, #ruby_options{cd=undefined, port_options=PortOptions,
                env=Env}} = ruby_options:parse([]),
            ?assertEqual([{env, Env}, {packet, 4}, binary, hide, exit_status],
                PortOptions)
        end,
        fun () ->
            {ok, #ruby_options{cd=TmpDir, port_options=PortOptions, env=Env}}
                = ruby_options:parse([{cd, TmpDir}]),
            ?assertEqual([{env, Env}, {packet, 4}, {cd, TmpDir},
                binary, hide, exit_status], PortOptions)
        end,
        ?_assertEqual({error, {invalid_option, {cd, "invalid_directory"}}},
            ruby_options:parse([{cd, "invalid_directory"}]))
    ] end}.

ruby_lib_option_test_() -> {setup,
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
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse([]),
            ?assertEqual(match, re:run(RubyLib, "/priv/ruby$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{ruby_lib, [TestPath1]}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{ruby_lib, TestPath1}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ "$", [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{ruby_lib, TestPath1 ++ ":" ++ TestPath2}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{ruby_lib, [TestPath1]},
                    {env, [{"RUBYLIB", TestPath2}]}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{env, [{"RUBYLIB", TestPath1},
                    {"RUBYLIB", TestPath2}]}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        fun () ->
            {ok, #ruby_options{ruby_lib=RubyLib,
                env=[{"RUBYLIB", RubyLib}]=Env,
                port_options=[{env, Env} | _]}} = ruby_options:parse(
                    [{ruby_lib, [TestPath1, TestPath2, ""]},
                    {env, [{"RUBYLIB", TestPath2 ++ ":" ++ TestPath1}]}]),
            ?assertEqual(match, re:run(RubyLib,
                "/priv/ruby:" ++ TestPath1 ++ ":" ++ TestPath2 ++ "$",
                [{capture, none}]))
        end,
        ?_assertEqual({error, {not_dir, UnknownPath}},
            ruby_options:parse([{ruby_lib, [TestPath1, UnknownPath]}])),
        ?_assertEqual({error, {invalid_option, {ruby_lib, invalid_path},
                not_list}},
            ruby_options:parse([{ruby_lib, invalid_path}])),
        ?_assertEqual({error, {invalid_option, {ruby_lib, ""},
                invalid_path}},
            ruby_options:parse([{ruby_lib, ""}])),
        ?_assertEqual({error, {invalid_option, {ruby_lib,
                [TestPath1, invalid]}, [invalid]}},
            ruby_options:parse([{ruby_lib, [TestPath1, invalid]}])),
        ?_assertEqual({error, {invalid_option, {ruby_lib,
                [$a, $b, invalid]}, [invalid]}},
            ruby_options:parse([{ruby_lib, [$a, $b, invalid]}])),
        fun () ->
            Dir = code:lib_dir(erlport),
            true = code:del_path(erlport),
            try ?assertEqual({error, {not_found, "erlport/priv"}},
                    ruby_options:parse([]))
            after
                true = code:add_patha(Dir)
            end
        end
    ] end}.

unknown_option_test_() ->
    ?_assertEqual({error, {unknown_option, unknown}},
        ruby_options:parse([unknown])).

create_mock_ruby(Dir, Name, Version) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path,
        <<"#! /bin/sh\necho 'ruby ",
        (list_to_binary(Version))/binary, " '\n">>, [raw]),
    ok = file:change_mode(Path, 8#00755),
    Path.
