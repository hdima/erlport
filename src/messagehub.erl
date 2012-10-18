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

-record(messagehub, {
    pid :: pid()
    }).

-record(state, {
    % TODO: Maybe value should be set()?
    % Topic -> ordset([Pid])
    subscribers = dict:new() :: dict()
    }).

% TODO: Need type spec for Python/Ruby subscribers (ErlPort Pid and
% function path)
-type message() :: term().
% TODO: Add Python/Ruby senders
-type sender() :: pid().
% TODO: Should it be only atoms?
-type topic() :: atom().
-type messagehub() :: #messagehub{}.
% TODO: Add #python{} and #ruby{}
-type subscriber() :: pid()
    | messagehub().

-define(IS_TOPIC(T), is_atom(T)).


%%
%% Start message hub
%%

-spec start() -> {ok, Hub::messagehub()} | {error, term()}.

start() ->
    start(start, pid).

-spec start(ServerName::{local | global, Name::atom()}) ->
    {ok, Hub::messagehub()} | {error, term()}.

start(ServerName) ->
    start(start, ServerName).


%%
%% Start linked message hub
%%

-spec start_link() -> {ok, Hub::messagehub()} | {error, term()}.

start_link() ->
    start(start_link, pid).

-spec start_link(ServerName::{local | global, Name::atom()}) ->
    {ok, Hub::messagehub()} | {error, term()}.

start_link(ServerName) ->
    start(start_link, ServerName).


%%
%% Stop message hub
%%

-spec stop(Hub::messagehub()) -> ok.

stop(#messagehub{pid=Pid}) ->
    gen_server:cast(Pid, stop).


%%
%% Subscribe subscriber on topic messages
%%

-spec subscribe(Hub::messagehub(), Topic::topic(), Subscriber::subscriber()) ->
    ok | {error, term()}.

subscribe(#messagehub{pid=Pid}, Topic, Subscriber) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {subscribe, Topic, Subscriber}).


%%
%% Unsubscribe subscriber from topic messages
%%

-spec unsubscribe(Hub::messagehub(), Topic::topic(),
        Subscriber::subscriber()) ->
    ok | {error, term()}.

unsubscribe(#messagehub{pid=Pid}, Topic, Subscriber) when ?IS_TOPIC(Topic) ->
    gen_server:call(Pid, {unsubscribe, Topic, Subscriber}).


%%
%% Send message for topic
%%

-spec send(Hub::messagehub(), Topic::topic(), Message::message(),
        Sender::sender()) ->
    ok.

send(#messagehub{pid=Pid}, Topic, Message, Sender) when ?IS_TOPIC(Topic) ->
    gen_server:cast(Pid, {send, Topic, Message, Sender}).

%%%
%%% Behaviour callbacks
%%%

init(undefined) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call({subscribe, Topic, Subscriber}, _From,
        State=#state{subscribers=Subscribers})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    NewSubscribers = add_subscriber(Topic, Subscriber, Subscribers),
    true = link(Subscriber),
    {reply, ok, State#state{subscribers=NewSubscribers}};
handle_call({unsubscribe, Topic, Subscriber}, _From,
        State=#state{subscribers=Subscribers})
        when ?IS_TOPIC(Topic) andalso is_pid(Subscriber) ->
    true = unlink(Subscriber),
    NewSubscribers = remove_subscriber(Topic, Subscriber, Subscribers),
    {reply, ok, State#state{subscribers=NewSubscribers}};
handle_call(Request, From, State) ->
    {reply, {error, {invalid_message, ?MODULE, From, Request}}, State}.


handle_cast({send, Topic, Message, Sender},
        State=#state{subscribers=Subscribers}) when ?IS_TOPIC(Topic) ->
    % TODO: Message should be defined by a record?
    send_messages(Topic, {topic_message, Sender, Topic, Message}, Subscribers),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', From, _Reason}, State=#state{subscribers=Subscribers}) ->
    % TODO: Log error or just ignore?
    NewSubscribers = remove_dead_subscriber(From, Subscribers),
    {noreply, State#state{subscribers=NewSubscribers}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Internal functions
%%%

remove_subscriber(Topic, Subscriber, Subscribers) ->
    case dict:find(Topic, Subscribers) of
        {ok, TopicSubscribers} ->
            case ordsets:del_element(Subscriber, TopicSubscribers) of
                TopicSubscribers ->
                    Subscribers;
                [] ->
                    dict:erase(Topic, Subscribers);
                Updated ->
                    dict:store(Topic, Updated, Subscribers)
            end;
        error ->
            Subscribers
    end.

add_subscriber(Topic, Subscriber, Subscribers) ->
    case dict:find(Topic, Subscribers) of
        {ok, TopicSubscribers} ->
            case ordsets:add_element(Subscriber, TopicSubscribers) of
                TopicSubscribers ->
                    Subscribers;
                Updated ->
                    dict:store(Topic, Updated, Subscribers)
            end;
        error ->
            dict:store(Topic, [Subscriber], Subscribers)
    end.

remove_dead_subscriber(Subscriber, Subscribers) ->
    % TODO: Remove empty list
    dict:map(fun (_Topic, TopicSubscribers) ->
            [P || P <- TopicSubscribers, P =/= Subscriber]
        end, Subscribers).

send_messages(Topic, Message, Subscribers) ->
    case dict:find(Topic, Subscribers) of
        {ok, TopicSubscribers} ->
            lists:foreach(fun (Pid) ->
                    Pid ! Message
                end, TopicSubscribers);
        error ->
            ok
    end.

start(Fun, ServerName) ->
    Result = case ServerName of
        pid ->
            gen_server:Fun(?MODULE, undefined, []);
        ServerName ->
            gen_server:Fun(ServerName, ?MODULE, undefined, [])
    end,
    case Result of
        {ok, Pid} ->
            {ok, #messagehub{pid=Pid}};
        Error ->
            Error
    end.
