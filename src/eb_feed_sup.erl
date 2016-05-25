%%
%% Copyright 2015-16 Joaquim Rocha
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

-module(eb_feed_sup).

-include("event_broker.hrl").
-include("eb_config.hrl").

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([create_feed/2, create_feed/3, drop_feed/1, list_feeds/0]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).
	
-spec create_feed(Name :: atom(), Filters :: list()) -> ok | {error, Reason :: term()}.
create_feed(Name, Filters) when is_atom(Name) andalso is_list(Filters) ->
	create_feed(Name, false, Filters).

-spec create_feed(Name :: atom(), Global :: boolean(), Filters :: list()) -> ok | {error, Reason :: term()}.
create_feed(Name, Global, Filters) when is_atom(Name) andalso is_boolean(Global) andalso is_list(Filters) ->
	case eb_config:find_feed(Name) of
		false ->
			case compile_filters(Filters) of
				{ok, REFilters} -> 
					{ok, _} = supervisor:start_child(?MODULE, [Name, Global]),
					eb_config:insert_feed(Name, REFilters),
					event_broker:publish(?EB_FEED_CREATED, Name);
				{error, Reason} -> {error, {invalid_filter, Reason}}
			end;
		_ -> {error, feed_already_exists}
	end.

-spec drop_feed(Name :: atom()) -> ok.
drop_feed(Name) when is_atom(Name) ->
	case eb_config:find_feed(Name) of
		false -> ok;
		{ok, _} ->
			Pid = whereis(Name),
			supervisor:terminate_child(?MODULE, Pid),
			eb_config:delete_feed(Name),
			event_broker:publish(?EB_FEED_DROPPED, Name),
			ok
	end.

-spec list_feeds() -> list().
list_feeds() ->
	Feeds = eb_config:list_feeds(),
	lists:map(fun(?FEED(Feed, _Filters)) -> Feed end, Feeds).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),
	Feed = {eb_feed, {eb_feed, start_link, []}, permanent, 2000, worker, [eb_feed]},
	{ok,{{simple_one_for_one, 10, 60}, [Feed]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

compile_filters([]) -> {error, empty_filter_list};
compile_filters(Filters) -> compile_filters(Filters, []).

compile_filters([], ReFilters) -> {ok, ReFilters};
compile_filters([Filter|T], ReFilters) ->
	case re:compile(Filter) of
		{ok, Re} -> compile_filters(T, [Re|ReFilters]);
		Other -> Other
	end.
