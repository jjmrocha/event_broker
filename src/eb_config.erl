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

-module(eb_config).

-include("eb_config.hrl").

-define(CONFIG_TABLE, event_broker_ets).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, drop/0]).
-export([insert/3, delete/1]).
-export([find/1, to_list/0, feed_server/1]).
-export([foldl/2]).

create() ->
	Options = [set, public, named_table],
	ets:new(?CONFIG_TABLE, Options).

drop() ->
	ets:delete(?CONFIG_TABLE).

insert(Feed, Filters, Pid) when is_atom(Feed) andalso is_list(Filters) andalso is_pid(Pid) ->
	ets:insert(?CONFIG_TABLE, ?FEED(Feed, Filters, Pid)).

delete(Feed) when is_atom(Feed) ->
	ets:delete(?CONFIG_TABLE, Feed).

find(Feed) when is_atom(Feed) ->
	case ets:lookup(?CONFIG_TABLE, Feed) of
		[] -> false;
		[FeedRecord] -> {ok, FeedRecord}
	end.

to_list() ->
	ets:tab2list(?CONFIG_TABLE).

feed_server(Feed) when is_atom(Feed) ->
	case find(Feed) of
		false -> false;
		{ok, FeedRecord} ->
			?FEED(_Feed, _Filters, Pid) = FeedRecord,
			{ok, Pid}
	end.

foldl(Function, Acc) ->
	ets:foldl(Function, Acc, ?CONFIG_TABLE).

%% ====================================================================
%% Internal functions
%% ====================================================================


