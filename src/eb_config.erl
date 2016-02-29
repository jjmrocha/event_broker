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

-module(eb_config).

-include("eb_config.hrl").

-define(FEED_TABLE, eb_config_feed_ets).
-define(ROUTING_TABLE, eb_config_routing_ets).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, drop/0]).
-export([insert_feed/2, delete_feed/1]).
-export([find_feed/1, list_feeds/0]).
-export([foldl_feeds/2]).
-export([event_routing/1, save_route/2]).

create() ->
	ets:new(?FEED_TABLE, [set, public, named_table]),
	ets:new(?ROUTING_TABLE, [set, public, named_table, {read_concurrency, true}]).

drop() ->
	ets:delete(?FEED_TABLE),
	ets:delete(?ROUTING_TABLE).

insert_feed(Feed, Filters) when is_atom(Feed) andalso is_list(Filters) ->
	ets:insert(?FEED_TABLE, ?FEED(Feed, Filters)),
	ets:delete_all_objects(?ROUTING_TABLE).

delete_feed(Feed) when is_atom(Feed) ->
	ets:delete(?FEED_TABLE, Feed),
	ets:delete_all_objects(?ROUTING_TABLE).

find_feed(Feed) when is_atom(Feed) ->
	case ets:lookup(?FEED_TABLE, Feed) of
		[] -> false;
		[FeedRecord] -> {ok, FeedRecord}
	end.

list_feeds() ->
	ets:tab2list(?FEED_TABLE).

foldl_feeds(Function, Acc) ->
	ets:foldl(Function, Acc, ?FEED_TABLE).

event_routing(EventName) when is_binary(EventName) ->
	case ets:lookup(?ROUTING_TABLE, EventName) of
		[] -> false;
		[?ROUTE(_, Feeds)] -> {ok, Feeds}
	end.	

save_route(EventName, Feeds) when is_binary(EventName) andalso is_list(Feeds) ->
	ets:insert(?ROUTING_TABLE, ?ROUTE(EventName, Feeds)).

%% ====================================================================
%% Internal functions
%% ====================================================================


