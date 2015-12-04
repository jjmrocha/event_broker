%%
%% Copyright 2015 Joaquim Rocha
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

-include("eb_config.hrl").

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([create_feed/2, drop_feed/1, list_feeds/0]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

-spec create_feed(Name :: binary(), Filters :: list()) -> ok | {error, Reason :: term()}.
create_feed(Name, Filters) when is_binary(Name) andalso is_list(Filters) ->
	case eb_config:find(Name) of
		false ->
			case compile_filters(Filters) of
				{ok, REFilters} -> supervisor:start_child(?MODULE, [Name, REFilters]);
				{error, Reason} -> {error, {invalid_filter, Reason}}
			end;
		_ -> {error, feed_already_exists}
	end.

-spec drop_feed(Name :: binary()) -> ok.
drop_feed(Name) when is_binary(Name) ->
	case eb_config:find(Name) of
		false -> ok;
		{ok, Pid} ->
			supervisor:terminate_child(?MODULE, Pid),
			eb_config:delete(Name),
			ok
	end.

-spec list_feeds() -> list().
list_feeds() ->
	Feeds = eb_config:to_list(),
	lists:map(fun(?FEED(Feed, _Filters, _Pid)) -> Feed end, Feeds).

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