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

-module(event_broker_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	{ok, Pid} = event_broker_sup:start_link(),
	eb_config:create(),
	{ok, Feed} = application:get_env(feeds),
	start_feeds(Feed, false),
	{ok, GlobalFeed} = application:get_env(global_feeds),
	start_feeds(GlobalFeed, true),	
	{ok, Pid}.

stop(_State) ->
	eb_config:drop(),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

start_feeds([], _Global) -> ok;
start_feeds([{Name, Filters}|T], Global) when is_atom(Name) andalso is_list(Filters) -> 
	eb_feed_sup:create_feed(Name, Global, Filters),
	start_feeds(T, Global);
start_feeds([H|T], Global) ->
	error_logger:error_msg("Invalid feed configuration: ~p\n", [H]),
	start_feeds(T, Global).
