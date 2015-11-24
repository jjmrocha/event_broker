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
-export([create_feed/2, drop_feed/1]).
-export([publish/1]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec publish(Event :: #event_record{}) -> ok.
publish(Event) when is_record(Event, event_record) ->
	worker_pool:cast(?MODULE, {publish, Event}).

-spec create_feed(Name :: binary(), Filters :: list()) -> ok | {error, Reason :: term()}.
create_feed(Name, Filters) when is_binary(Name) andalso is_list(Filters) ->
	case eb_config:find(Name) of
		false ->
			case compile_filters(Filters) of
				{ok, REFilters} ->
					case eb_feed_sup:start_feed() of
						{ok, Pid} -> 
							eb_config:insert(Name, REFilters, Pid),
							error_logger:info_msg("Feed ~s [~p] is started...\n", [Name, Pid]),
							ok;
						_ -> {error, internal_error}
					end;
				{error, Reason} ->
					{error, {invalid_filter, Reason}}
			end;
		_ -> {error, feed_already_exists}
	end.

-spec drop_feed(Name :: binary()) -> ok.
drop_feed(Name) when is_binary(Name) ->
	case eb_config:find(Name) of
		false -> ok;
		{ok, Pid} ->
			eb_feed_sup:stop_feed(Pid),
			eb_config:delete(Name),
			ok
	end.

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

compile_filters([]) -> {error, empty_filter_list};
compile_filters(Filters) -> compile_filters(Filters, []).

compile_filters([], ReFilters) -> {ok, ReFilters};
compile_filters([Filter|T], ReFilters) ->
	case re:compile(Filter) of
		{ok, Re} -> compile_filters(T, [Re|ReFilters]);
		Other -> Other
	end.

route(Event) ->
	Feeds = eb_config:foldl(fun(?FEED(_Feed, Filters, Pid), Acc) ->
					case matches(Event#event_record.name, Filters) of
						false -> Acc;
						true -> [Pid|Acc]
					end
			end,[]),
	lists:foreach(fun(Pid) -> 
				eb_feed:publish(Pid, Event) 
		end, Feeds).

matches(_Name, []) -> false;
matches(Name, [Re|T]) ->
	case re:run(Name, Re) of
		{match, _} -> true;
		_ -> matches(Name, T)
	end.
