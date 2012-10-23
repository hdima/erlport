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

-module(messagehub_tests).

-include_lib("eunit/include/eunit.hrl").


setup() ->
    {ok, H} = messagehub:start_link(),
    H.

cleanup(H) ->
    ok = messagehub:stop(H).

start_stop_test_() -> [
    fun () ->
        {ok, H} = messagehub:start(),
        ?assertEqual(ok, messagehub:stop(H))
    end,
    fun () ->
        {ok, H} = messagehub:start_link(),
        ?assertEqual(ok, messagehub:stop(H))
    end,
    fun () ->
        ?assertMatch({ok, _}, messagehub:start({local, messagehub_test})),
        ?assertEqual(ok, messagehub:stop(messagehub_test))
    end,
    fun () ->
        ?assertMatch({ok, _}, messagehub:start_link({local, messagehub_test})),
        ?assertEqual(ok, messagehub:stop(messagehub_test))
    end
    ].

subscribe_topic_test_() -> {foreach,
    fun setup/0,
    fun cleanup/1, [
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([{subscribed, H, test}], get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([{subscribed, H, test}], get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([{subscribed_all, H}, {unsubscribed_all, H},
                    {subscribed, H, test}], get_messages())
            end
        end
    ]}.

unsubscribe_topic_test_() -> {foreach,
    fun setup/0,
    fun cleanup/1, [
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:unsubscribe(H, S, test)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([{subscribed, H, test}, {unsubscribed, H, test}],
                    get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual(ok, messagehub:unsubscribe(H, S, test)),
                ?assertEqual(ok, messagehub:unsubscribe(H, S, test)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([{subscribed, H, test}, {unsubscribed, H, test}],
                    get_messages())
            end
        end
    ]}.

subscribe_all_test_() -> {foreach,
    fun setup/0,
    fun cleanup/1, [
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed_all, H}], get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed_all, H}], get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed, H, test}, {unsubscribed, H, test},
                    {subscribed_all, H}], get_messages())
            end
        end
    ]}.

unsubscribe_all_test_() -> {foreach,
    fun setup/0,
    fun cleanup/1, [
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:unsubscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed_all, H}, {unsubscribed_all, H}],
                    get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:unsubscribe_all(H, S)),
                ?assertEqual(ok, messagehub:unsubscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed_all, H}, {unsubscribed_all, H}],
                    get_messages())
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual([test], messagehub:get_topics(H)),
                ?assertEqual([S], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual(ok, messagehub:unsubscribe_all(H, S)),
                ?assertEqual([], messagehub:get_topics(H)),
                ?assertEqual([], messagehub:get_topic_subscribers(H, test)),
                ?assertEqual([], messagehub:get_all_subscribers(H)),
                ?assertEqual([{subscribed, H, test}, {unsubscribed, H, test}],
                    get_messages())
            end
        end
    ]}.

send_message_test_() -> {foreach,
    fun setup/0,
    fun cleanup/1, [
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual(ok, messagehub:send(H, "TEST", test)),
                ?assertEqual([{subscribed, H, test},
                    {topic_message, "TEST", test, [H, S]}], get_messages(3000))
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual(ok, messagehub:subscribe(H, S, test)),
                ?assertEqual(ok, messagehub:send(H, "TEST", test)),
                ?assertEqual(ok, messagehub:send(H, "TEST2", other)),
                ?assertEqual([{subscribed, H, test},
                    {topic_message, "TEST", test, [H, S]}], get_messages(3000))
            end
        end,
        fun (H) ->
            fun () ->
                S = self(),
                ?assertEqual(ok, messagehub:subscribe_all(H, S)),
                ?assertEqual(ok, messagehub:send(H, "TEST", test)),
                ?assertEqual([{subscribed_all, H},
                    {topic_message, "TEST", test, [H, S]}], get_messages(3000))
            end
        end
    ]}.

get_messages() ->
    get_messages(0).

get_messages(Timeout) when is_integer(Timeout) andalso Timeout >=0 ->
    get_messages([], Timeout).

get_messages(Messages, Timeout) ->
    receive
        M ->
            get_messages([M | Messages], Timeout)
    after
        Timeout ->
            lists:reverse(Messages)
    end.
