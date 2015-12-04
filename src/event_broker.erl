%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(event_broker).

-include("event_broker.hrl").
-include("eb_config.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([publish/1]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec publish(Event :: #event_record{}) -> ok.
publish(Event) when is_record(Event, event_record) ->
	worker_pool:cast(?MODULE, {publish, Event}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
%% init/1
init([]) ->
	{ok, none}.

%% handle_call/3
handle_call(Request, _From, State) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, State}.

%% handle_cast/2
handle_cast({publish, Event}, State) ->
	route(Event),
	{noreply, State};
handle_cast(Msg, State) ->
	error_logger:info_msg("~p(~p): Unexpected cast message ~p\n", [?MODULE, self(), Msg]),
	{noreply, State}.

%% handle_info/2
handle_info(Info, State) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{noreply, State}.

%% terminate/2
terminate(_Reason, _State) ->
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

route(Event) ->
	Feeds = find_feeds(Event#event_record.name),
	lists:foreach(fun(Pid) -> 
				eb_feed:publish(Pid, Event) 
		end, Feeds).

find_feeds(EventName) ->
	case eb_config:event_routing(EventName) of
		false ->
			Feeds = eb_config:foldl(fun(?FEED(_Feed, Filters, Pid), Acc) ->
							case matches(EventName, Filters) of
								false -> Acc;
								true -> [Pid|Acc]
							end
					end,[]),
			eb_config:save_route(EventName, Feeds),
			Feeds;
		{ok, Feeds} -> Feeds
	end.

matches(_Name, []) -> false;
matches(Name, [Re|T]) ->
	case re:run(Name, Re) of
		{match, _} -> true;
		_ -> matches(Name, T)
	end.
