%%
%% Copyright 2015-16 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(eb_subscription).

-include("event_broker.hrl").

-behaviour(eb_event_handler).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([subscribe/1, unsubscribe/1]).
-export([count/1]).

-spec subscribe(Feed :: atom()) -> ok | {error, Reason :: term()}.
subscribe(Feed) when is_atom(Feed) ->
	check_and_setup(Feed),
	Subscriber = self(),
	eb_feed:call(Feed, ?MODULE, {subscribe, Subscriber}).	

-spec unsubscribe(Feed :: atom()) -> ok | {error, Reason :: term()}.
unsubscribe(Feed) when is_atom(Feed) ->
	iff_registered(Feed, fun() ->
			Subscriber = self(),
			eb_feed:call(Feed, ?MODULE, {unsubscribe, Subscriber})
		end, ok).

-spec count(Feed :: atom()) -> {ok, Count :: integer()} | {error, Reason :: term()}.
count(Feed) when is_atom(Feed) ->
	iff_registered(Feed, fun() -> 
			eb_feed:call(Feed, ?MODULE, {count}) 
		end, {ok, 0}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([]) ->
	{ok, dict:new()}.

%% handle_event/2
handle_event(Event, Dict) ->
	Notification = ?NOTIFICATION(Event),
	lists:foreach(fun(Pid) ->
				Pid ! Notification
		end, dict:fetch_keys(Dict)),
	{ok, Dict}.

%% handle_call/2
handle_call({subscribe, Pid}, Dict) ->
	Dict1 = case dict:is_key(Pid, Dict) of
		true -> Dict;
		false ->
			Ref = erlang:monitor(process, Pid),
			dict:store(Pid, Ref, Dict)
	end,
	{ok, ok, Dict1};
handle_call({unsubscribe, Pid}, Dict) ->
	Dict1 = case dict:find(Pid, Dict) of
		{ok, Ref} -> 
			erlang:demonitor(Ref),
			dict:erase(Pid, Dict);
		false -> Dict
	end,
	{ok, ok, Dict1};
handle_call({count}, Dict) ->
	Size = dict:size(Dict),
	{ok, {ok, Size}, Dict};
handle_call(Request, State) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, State}.

%% handle_info/2
handle_info({'DOWN', _, _, Pid, _}, Dict) ->
	Dict1 = dict:erase(Pid, Dict),
	{ok, Dict1};
handle_info(Info, Dict) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{ok, Dict}.

%% terminate/2
terminate(Dict) ->
	lists:foreach(fun({_, Ref}) -> 
				erlang:demonitor(Ref) 
		end, 
		dict:to_list(Dict)).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_and_setup(Feed) ->
	case is_registered(Feed) of
		false -> eb_feed:register(Feed, ?MODULE, []);
		true -> ok
	end.

iff_registered(Feed, Fun, Else) ->
	case is_registered(Feed) of
		true -> Fun();
		false -> Else
	end.

is_registered(Feed) -> eb_feed:is_registered(Feed, ?MODULE).
