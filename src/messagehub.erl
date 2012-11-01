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
    send/3,
    send/5,
    subscribe/3,
    unsubscribe/3,
    subscribe_all/2,
    unsubscribe_all/2
    ]).

-export([
    get_topics/1,
    get_topic_subscribers/2,
    get_all_subscribers/1
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

-include("messages.hrl").

-record(state, {
    all = ordsets:new() :: ordsets:ordset(pid()),
    % Topic -> ordsets([Pid])
    topics = dict:new() :: dict(),
    % Pid -> ordsets([Topic])
    subscribers = dict:new() :: dict(),
    % Pid -> Monitor::reference()
    monitors = dict:new() :: dict()
    }).

-type server_name() :: {local, Name::atom()}
    | {global, GlobalName::term()}
    | {via, Module::atom(), ViaName::term()}.
% TODO: Need type spec for Python/Ruby subscribers (ErlPort Pid and
% function path)
-type payload() :: term().
% TODO: How to represent external functions?
-type sender() :: [pid()].
-type topic() :: atom().
% TODO: How to represent external functions?
-type destination() :: [pid()].
-type subscriber() :: pid().
-type hub() :: erlport:server_instance().
-type send_options() :: [route_by_path].

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

-spec start(ServerName::server_name()) ->
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

-spec start_link(ServerName::server_name()) ->
    {ok, Hub::pid()} | {error, term()}.

start_link(ServerName) ->
    start(start_link, ServerName).


%%
%% @doc Stop message hub
%%

-spec stop(Hub::hub()) -> ok.

stop(Pid) ->
    gen_server:cast(Pid, stop).


%%
%% @doc Subscribe subscriber on topic messages
%%

-spec subscribe(Hub::hub(), Subscriber::subscriber(), Topic::topic()) ->
    ok | {error, term()}.

subscribe(Pid, Subscriber, Topic) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {subscribe, Subscriber, Topic}).


%%
%% @doc Unsubscribe subscriber from topic messages
%%

-spec unsubscribe(Hub::hub(), Subscriber::subscriber(), Topic::topic()) ->
    ok | {error, term()}.

unsubscribe(Pid, Subscriber, Topic) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {unsubscribe, Subscriber, Topic}).

%%
%% @doc Subscriber subscriber to all messages
%%

-spec subscribe_all(Hub::hub(), Subscriber::subscriber()) ->
    ok | {error, term()}.

subscribe_all(Pid, Subscriber) ->
    gen_server:call(Pid, {subscribe_all, Subscriber}).

%%
%% @doc Unsubscriber subscriber from all messages
%%

-spec unsubscribe_all(Hub::hub(), Subscriber::subscriber()) ->
    ok | {error, term()}.

unsubscribe_all(Pid, Subscriber) ->
    gen_server:call(Pid, {unsubscribe_all, Subscriber}).

%%
%% @doc Send message for topic or to selected destination
%%

-spec send(Hub::hub(), Payload::payload(), Topic::topic() | destination()) ->
    ok.

send(Hub, Payload, Topic) ->
    send(Hub, Payload, Topic, [Hub, self()], []).

%%
%% @doc Send message for topic or to selected destination with explicit sender
%%

-spec send(Hub::hub(), Payload::payload(), Topic::topic() | destination(),
        Sender::sender(), Options::send_options()) ->
    ok.

send(Hub, Payload, Topic, Sender=[_|_], Options) when ?IS_TOPIC(Topic)
        andalso is_list(Options) ->
    gen_server:cast(Hub, {send, Payload, Topic, Sender, Options});
send(Hub, Payload, Destination=[_|_], Sender=[_|_], Options)
        when is_list(Options) ->
    case Options of
        [] ->
            Dest = lists:last(Destination),
            do_send(Dest, [Hub | Sender], [Dest], Payload);
        [route_by_path] ->
            case Destination of
                [Next] ->
                    do_send(Next, [Hub | Sender], [Next], Payload);
                [Next | Tail] ->
                    do_send(Next, [Hub | Sender], Tail, Payload)
            end
    end.

%%
%% @doc Return all registered topics
%%

-spec get_topics(Hub::hub()) -> [Topic::topic()].

get_topics(Hub) ->
    gen_server:call(Hub, get_topics).

%%
%% @doc Return all subscribers for the topic
%%

-spec get_topic_subscribers(Hub::hub(), Topic::topic()) ->
    [Subscriber::subscriber()].

get_topic_subscribers(Hub, Topic) when ?IS_TOPIC(Topic) ->
    gen_server:call(Hub, {get_topic_subscribers, Topic}).

%%
%% @doc Return subscribers for all messages
%%

-spec get_all_subscribers(Hub::hub()) ->
    [Subscriber::subscriber()].

get_all_subscribers(Hub) ->
    gen_server:call(Hub, get_all_subscribers).

%%%
%%% Behaviour callbacks
%%%

init(undefined) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call({subscribe, Subscriber, Topic}, _From, State=#state{})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    {reply, ok, do_subscribe(Subscriber, Topic, true, State)};
handle_call({unsubscribe, Subscriber, Topic}, _From, State=#state{})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    {reply, ok, do_unsubscribe(Subscriber, Topic, true, State)};
handle_call({subscribe_all, Subscriber}, _From, State=#state{})
        when is_pid(Subscriber) ->
    {reply, ok, do_subscribe_all(Subscriber, true, State)};
handle_call({unsubscribe_all, Subscriber}, _From, State=#state{})
        when is_pid(Subscriber) ->
    {reply, ok, do_unsubscribe_all(Subscriber, true, State)};
handle_call(get_topics, _From, State=#state{topics=Topics}) ->
    {reply, dict:fetch_keys(Topics), State};
handle_call({get_topic_subscribers, Topic}, _From,
        State=#state{topics=Topics}) ->
    {reply, get_all_values(Topic, Topics), State};
handle_call(get_all_subscribers, _From, State=#state{all=All}) ->
    {reply, All, State};
handle_call(Request, From, State) ->
    {reply, {error, {invalid_message, ?MODULE, From, Request}}, State}.


handle_cast({send, Payload, Topic, Sender=[_|_], Options},
        State=#state{topics=Topics, all=All}) when ?IS_TOPIC(Topic)
        andalso is_list(Options) ->
    send_messages(All, Sender, Topic, Payload),
    send_messages(Topic, Topics, Sender, Topic, Payload),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(#message{destination=Destination, sender=Sender, payload=Payload},
        State=#state{topics=Topics, all=All}) ->
    case Destination of
        [Self] when Self == self() ->
            % Ignore. Message Hub doesn't support message processing
            ok;
        [Next | Tail] ->
            do_send(Next, [self() | Sender], Tail, Payload);
        Topic when ?IS_TOPIC(Topic) ->
            send_messages(All, [self() | Sender], Destination, Payload),
            send_messages(Topic, Topics, [self() | Sender], Destination,
                Payload)
    end,
    {noreply, State};
handle_info(#subscribed{hub=Hub, topic=Topic}, State=#state{}) ->
    % TODO: We don't need to unsubscribe in this case
    {noreply, do_subscribe(Hub, Topic, false, State)};
handle_info(#unsubscribed{hub=Hub, topic=Topic}, State=#state{}) ->
    % TODO: We don't need to unsubscribe topics in this case
    {noreply, do_unsubscribe(Hub, Topic, false, State)};
handle_info(#subscribed_all{hub=Hub}, State=#state{}) ->
    % TODO: We don't need to unsubscribe in this case
    {noreply, do_subscribe_all(Hub, false, State)};
handle_info(#unsubscribed_all{hub=Hub}, State=#state{}) ->
    % TODO: We don't need to unsubscribe topics in this case
    {noreply, do_unsubscribe_all(Hub, false, State)};
handle_info({'DOWN', _Monitor, process, Pid, _Info},
        State=#state{topics=Topics, subscribers=Subscribers, all=All,
            monitors=Monitors}) ->
    {SubscriptionTopics, NewSubscribers} = pop_all_values(Pid, Subscribers),
    NewTopics = remove_element_by_list(Pid, SubscriptionTopics, Topics),
    NewAll = ordsets:del_element(Pid, All),
    NewMonitors = dict:erase(Pid, Monitors),
    {noreply, State#state{topics=NewTopics, subscribers=NewSubscribers,
        all=NewAll, monitors=NewMonitors}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

do_subscribe(Subscriber, Topic, Notify, State=#state{topics=Topics, all=All,
        subscribers=Subscribers, monitors=Monitors}) ->
    NewTopics = add_element(Topic, Subscriber, Topics),
    NewAll = case ordsets:del_element(Subscriber, All) of
        All ->
            All;
        New ->
            notify(Subscriber, #unsubscribed_all{hub=self()}, Notify),
            New
    end,
    {NewSubscribers, NewMonitors} = case add_element(Subscriber, Topic,
            Subscribers) of
        Subscribers ->
            {Subscribers, Monitors};
        New2 ->
            Monitor = monitor(process, Subscriber),
            notify(Subscriber, #subscribed{hub=self(), topic=Topic}, Notify),
            {New2, dict:store(Subscriber, Monitor, Monitors)}
    end,
    State#state{topics=NewTopics, all=NewAll,
        subscribers=NewSubscribers, monitors=NewMonitors}.

do_unsubscribe(Subscriber, Topic, Notify, State=#state{topics=Topics,
        subscribers=Subscribers, monitors=Monitors}) ->
    NewTopics = remove_element(Topic, Subscriber, Topics),
    {NewSubscribers, NewMonitors} = case remove_element(Subscriber, Topic,
            Subscribers) of
        Subscribers ->
            {Subscribers, Monitors};
        New ->
            notify(Subscriber, #unsubscribed{hub=self(), topic=Topic}, Notify),
            {New, demonitor_subscriber(Subscriber, Monitors)}
    end,
    State#state{topics=NewTopics, subscribers=NewSubscribers,
        monitors=NewMonitors}.

do_subscribe_all(Subscriber, Notify, State=#state{all=All, topics=Topics,
        subscribers=Subscribers, monitors=Monitors}) ->
    {SubscriptionTopics, NewSubscribers} = pop_all_values(
        Subscriber, Subscribers),
    NewTopics = case remove_element_by_list(Subscriber,
            SubscriptionTopics, Topics) of
        Topics ->
            Topics;
        New ->
            % TODO: Maybe it's enough to send only {unsubscribed_all} message?
            notify_for_topics(Subscriber, SubscriptionTopics, Notify),
            New
    end,
    {NewAll, NewMonitors} = case ordsets:add_element(Subscriber, All) of
        All ->
            {All, Monitors};
        New2 ->
            Monitor = monitor(process, Subscriber),
            notify(Subscriber, #subscribed_all{hub=self()}, Notify),
            {New2, dict:store(Subscriber, Monitor, Monitors)}
    end,
    State#state{topics=NewTopics, subscribers=NewSubscribers, all=NewAll,
        monitors=NewMonitors}.

do_unsubscribe_all(Subscriber, Notify, State=#state{all=All, topics=Topics,
        subscribers=Subscribers, monitors=Monitors}) ->
    {NewAll, NewMonitors} = case ordsets:del_element(Subscriber, All) of
        All ->
            {All, Monitors};
        New ->
            notify(Subscriber, #unsubscribed_all{hub=self()}, Notify),
            {New, demonitor_subscriber(Subscriber, Monitors)}
    end,
    {SubscriptionTopics, NewSubscribers} = pop_all_values(
        Subscriber, Subscribers),
    NewTopics = case remove_element_by_list(Subscriber,
            SubscriptionTopics, Topics) of
        Topics ->
            Topics;
        New2 ->
            % TODO: Maybe it's enough to send only {unsubscribed_all} message?
            notify_for_topics(Subscriber, SubscriptionTopics, Notify),
            New2
    end,
    State#state{topics=NewTopics, subscribers=NewSubscribers, all=NewAll,
        monitors=NewMonitors}.

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

pop_all_values(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Values} ->
            {Values, dict:erase(Key, Dict)};
        error ->
            {[], Dict}
    end.

get_all_values(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Values} ->
            Values;
        error ->
            []
    end.

remove_element_by_list(Element, List, Dict) ->
    lists:foldl(fun (E, D) ->
            remove_element(E, Element, D)
        end, Dict, List).

send_messages(Topic, Subscribers, Sender, Destination, Payload) ->
    case dict:find(Topic, Subscribers) of
        {ok, TopicSubscribers} ->
            send_messages(TopicSubscribers, Sender, Destination, Payload);
        error ->
            ok
    end.

send_messages(Subscribers, Sender, Destination, Payload) ->
    lists:foreach(fun (Next) ->
            do_send(Next, Sender, Destination, Payload)
        end, Subscribers).

start(Function, ServerName) ->
    case ServerName of
        pid ->
            gen_server:Function(?MODULE, undefined, []);
        ServerName ->
            gen_server:Function(ServerName, ?MODULE, undefined, [])
    end.

demonitor_subscriber(Subscriber, Monitors) ->
    case dict:find(Subscriber, Monitors) of
        {ok, Monitor} ->
            true = demonitor(Monitor),
            dict:erase(Subscriber, Monitors);
        error ->
            Monitors
    end.

notify_for_topics(Subscriber, Topics, true) ->
    lists:foreach(fun (Topic) ->
        Subscriber ! #unsubscribed{hub=self(), topic=Topic}
        end, Topics);
notify_for_topics(_Subscriber, _Topics, false) ->
    ok.

notify(Subscriber, Message, true) ->
    Subscriber ! Message;
notify(_Subscriber, _Message, false) ->
    ok.

do_send(Next, Sender, Destination, Payload) ->
    case is_routing_loop(Next, Sender) of
        false ->
            Next ! #message{sender=Sender, destination=Destination,
                payload=Payload};
        true ->
            % Ignore. Routing loop
            ok
    end.

is_routing_loop(Next, [Next]) ->
    % Sender can send messages to itself
    false;
is_routing_loop(Next, [Next | _]) ->
    true;
is_routing_loop(Next, [_ | Tail]) ->
    is_routing_loop(Next, Tail);
is_routing_loop(_, []) ->
    false.
