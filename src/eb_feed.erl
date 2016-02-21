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

-module(eb_feed).

-include("event_broker.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).
-export([register/3, unregister/2, list_handlers/1]).
-export([publish/2]).

start_link(Name, REFilters) ->
	{ok, Pid} = gen_event:start_link(),
	eb_config:insert(Name, REFilters, Pid),
	error_logger:info_msg("Feed ~s [~p] is started...\n", [Name, Pid]),
	gen_event:add_handler(Pid, eb_subscription, []),
	{ok, Pid}.

-spec register(Feed :: binary(), Handler :: gen_event:handler(), Args :: term()) -> ok | {error, Reason :: term()}.
register(Feed, Handler, Args) ->
	case eb_config:feed_server(Feed) of
		false -> {error, feed_not_found};
		{ok, Pid} -> 
			case gen_event:add_handler(Pid, Handler, Args) of
				ok ->
					error_logger:info_msg("Handler ~p added to feed ~s with args: ~p\n", [Handler, Feed, Args]),
					ok;
				_ -> {error, handler_not_registered}
			end
	end.

-spec unregister(Feed :: binary(), Handler :: gen_event:handler()) -> ok | {error, Reason :: term()}.
unregister(Feed, Handler) ->
	case eb_config:feed_server(Feed) of
		false -> {error, feed_not_found};
		{ok, Pid} -> 
			case gen_event:delete_handler(Pid, Handler, []) of
				ok ->
					error_logger:info_msg("Handler ~p deleted from feed ~s\n", [Handler, Feed]),
					ok;
				_ -> {error, handler_not_unregistered}
			end
	end.	

-spec list_handlers(Feed :: binary()) -> {ok, Handlers :: list()} | {error, Reason :: term()}.
list_handlers(Feed) ->
	case eb_config:feed_server(Feed) of
		false -> {error, feed_not_found};
		{ok, Pid} -> 
			Handlers = gen_event:which_handlers(Pid),
			{ok, lists:delete(eb_subscription, Handlers)}
	end.

-spec publish(Pid :: pid(), Event :: #event_record{}) -> ok.
publish(Pid, Event) when is_record(Event, event_record) ->
	gen_event:notify(Pid, Event).

%% ====================================================================
%% Internal functions
%% ====================================================================

