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

-module(event_broker).

-include("event_broker.hrl").
-include("eb_config.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([publish/1, publish/2, publish/3]).

-spec publish(Event :: #event_record{}) -> ok.
publish(Event) when is_record(Event, event_record) ->
	route(Event).

-spec publish(Name::binary(), Ref::term()) -> ok.
publish(Name, Ref) when is_binary(Name) ->
	Event = eb_event:new(Name, Ref),
	publish(Event).

-spec publish(Name::binary(), Ref::term(), Info::map()) -> ok.
publish(Name, Ref, Info) when is_binary(Name) andalso is_map(Info) ->
	Event = eb_event:new(Name, Ref, Info),
	publish(Event).

%% ====================================================================
%% Internal functions
%% ====================================================================

route(Event) ->
	Feeds = find_feeds(Event#event_record.name),
	lists:foreach(fun(Feed) -> 
				eb_feed:publish(Feed, Event) 
		end, Feeds).

find_feeds(EventName) ->
	case eb_config:event_routing(EventName) of
		false ->
			Feeds = eb_config:foldl_feeds(fun(?FEED(Feed, Filters), Acc) ->
							case matches(EventName, Filters) of
								false -> Acc;
								true -> [Feed|Acc]
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
