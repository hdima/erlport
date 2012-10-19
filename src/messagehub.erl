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

-module(messagehub).

-author('Dmitry Vasiliev <dima@hlabs.org>').

-behaviour(gen_server).

-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    stop/1,
    subscribe/3,
    unsubscribe/3,
    subscribe_all/2,
    unsubscribe_all/2,
    send/4
    ]).

%% Behaviour callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-record(state, {
    all = ordsets:new() :: ordsets:ordset(pid()),
    % Topic -> ordsets([Pid])
    topics = dict:new() :: dict(),
    % Pid -> ordsets([Topic])
    subscribers = dict:new() :: dict()
    }).

% TODO: Need type spec for Python/Ruby subscribers (ErlPort Pid and
% function path)
-type message() :: term().
-type sender() :: pid().
% TODO: Should it be only atoms?
-type topic() :: atom().
-type subscriber() :: pid().

-define(IS_TOPIC(T), is_atom(T)).


%%
%% @doc Start message hub
%%

-spec start() -> {ok, Hub::pid()} | {error, term()}.

start() ->
    start(start, pid).

%%
%% @doc Start named message hub
%%

-spec start(ServerName::erlport:server_name()) ->
    {ok, Hub::pid()} | {error, term()}.

start(ServerName) ->
    start(start, ServerName).

%%
%% @doc Start linked message hub
%%

-spec start_link() -> {ok, Hub::pid()} | {error, term()}.

start_link() ->
    start(start_link, pid).

%%
%% @doc Start named and linked message hub
%%

-spec start_link(ServerName::erlport:server_name()) ->
    {ok, Hub::pid()} | {error, term()}.

start_link(ServerName) ->
    start(start_link, ServerName).


%%
%% @doc Stop message hub
%%

-spec stop(Hub::erlport:server_instance()) -> ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).


%%
%% @doc Subscribe subscriber on topic messages
%%

-spec subscribe(Hub::erlport:server_instance(), Topic::topic(),
        Subscriber::subscriber()) ->
    ok | {error, term()}.

subscribe(Pid, Topic, Subscriber) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {subscribe, Topic, Subscriber}).


%%
%% @doc Unsubscribe subscriber from topic messages
%%

-spec unsubscribe(Hub::erlport:server_instance(), Topic::topic(),
        Subscriber::subscriber()) ->
    ok | {error, term()}.

unsubscribe(Pid, Topic, Subscriber) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {unsubscribe, Topic, Subscriber}).

%%
%% @doc Subscriber subscriber to all messages
%%

-spec subscribe_all(Hub::erlport:server_instance(),
        Subscriber::subscriber()) ->
    ok | {error, term()}.

subscribe_all(Pid, Subscriber) ->
    gen_server:call(Pid, {subscribe_all, Subscriber}).

%%
%% @doc Unsubscriber subscriber from all messages
%%

-spec unsubscribe_all(Hub::erlport:server_instance(),
        Subscriber::subscriber()) ->
    ok | {error, term()}.

unsubscribe_all(Pid, Subscriber) ->
    gen_server:call(Pid, {unsubscribe_all, Subscriber}).

%%
%% @doc Send message for topic
%%

-spec send(Hub::erlport:server_instance(), Topic::topic(), Message::message(),
        Sender::sender()) ->
    ok.

send(Pid, Topic, Message, Sender) when ?IS_TOPIC(Topic) ->
    gen_server:cast(Pid, {send, Topic, Message, Sender}).

%%%
%%% Behaviour callbacks
%%%

init(undefined) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call({subscribe, Topic, Subscriber}, _From,
        State=#state{topics=Topics, all=All, subscribers=Subscribers})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    NewTopics = add_element(Topic, Subscriber, Topics),
    NewAll = remove_set_element(Subscriber, All),
    NewSubscribers = add_element(Topic, Subscriber, Subscribers),
    true = link(Subscriber),
    {reply, ok, State#state{topics=NewTopics, all=NewAll,
        subscribers=NewSubscribers}};
handle_call({unsubscribe, Topic, Subscriber}, _From,
        State=#state{topics=Topics, subscribers=Subscribers})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    true = unlink(Subscriber),
    NewTopics = remove_element(Topic, Subscriber, Topics),
    NewSubscribers = remove_element(Topic, Subscriber, Subscribers),
    {reply, ok, State#state{topics=NewTopics, subscribers=NewSubscribers}};
handle_call({subscribe_all, Subscriber}, _From,
        State=#state{all=All, topics=Topics, subscribers=Subscribers}) ->
    {SubscriptionTopics, NewSubscribers} = pop_all_values(
        Subscriber, Subscribers),
    NewTopics = remove_element_by_list(Subscriber, SubscriptionTopics, Topics),
    NewAll = add_set_element(Subscriber, All),
    {reply, ok, State#state{topics=NewTopics, subscribers=NewSubscribers,
        all=NewAll}};
handle_call({unsubscribe_all, Subscriber}, _From,
        State=#state{all=All, topics=Topics, subscribers=Subscribers}) ->
    NewAll = remove_set_element(Subscriber, All),
    {SubscriptionTopics, NewSubscribers} = pop_all_values(
        Subscriber, Subscribers),
    NewTopics = remove_element_by_list(Subscriber, SubscriptionTopics, Topics),
    {reply, ok, State#state{topics=NewTopics, subscribers=NewSubscribers,
        all=NewAll}};
handle_call(Request, From, State) ->
    {reply, {error, {invalid_message, ?MODULE, From, Request}}, State}.


handle_cast({send, Topic, Payload, Sender},
        State=#state{topics=Topics, all=All}) when ?IS_TOPIC(Topic) ->
    % TODO: Message should be defined by a record?
    % TODO: Sender should inside list?
    Message = {topic_message, Sender, Topic, Payload},
    send_messages(Message, All),
    send_messages(Topic, Message, Topics),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', From, _Reason}, State=#state{topics=Topics,
        subscribers=Subscribers, all=All}) ->
    % TODO: Log error or just ignore?
    {SubscriptionTopics, NewSubscribers} = pop_all_values(From, Subscribers),
    NewTopics = remove_element_by_list(From, SubscriptionTopics, Topics),
    NewAll = remove_set_element(From, All),
    {noreply, State#state{topics=NewTopics, subscribers=NewSubscribers,
        all=NewAll}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

remove_element(Key, Element, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Values} ->
            case ordsets:del_element(Element, Values) of
                Values ->
                    Dict;
                [] ->
                    dict:erase(Key, Dict);
                Updated ->
                    dict:store(Key, Updated, Dict)
            end;
        error ->
            Dict
    end.

add_element(Key, New, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Values} ->
            case ordsets:add_element(New, Values) of
                Values ->
                    Dict;
                Updated ->
                    dict:store(Key, Updated, Dict)
            end;
        error ->
            dict:store(Key, [New], Dict)
    end.

remove_set_element(Element, Set) ->
    ordsets:del_element(Element, Set).

add_set_element(Element, Set) ->
    ordsets:add_element(Element, Set).

pop_all_values(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Values} ->
            {Values, dict:erase(Key, Dict)};
        error ->
            {[], Dict}
    end.

remove_element_by_list(Element, List, Dict) ->
    lists:foldl(fun (E, D) ->
            remove_element(E, Element, D)
        end, Dict, List).

send_messages(Topic, Message, Subscribers) ->
    case dict:find(Topic, Subscribers) of
        {ok, TopicSubscribers} ->
            send_messages(Message, TopicSubscribers);
        error ->
            ok
    end.

send_messages(Message, Subscribers) ->
    lists:foreach(fun (Pid) ->
            Pid ! Message
        end, Subscribers).

start(Function, ServerName) ->
    case ServerName of
        pid ->
            gen_server:Function(?MODULE, undefined, []);
        ServerName ->
            gen_server:Function(ServerName, ?MODULE, undefined, [])
    end.
