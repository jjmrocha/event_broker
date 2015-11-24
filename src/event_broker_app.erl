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

-module(event_broker_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	{ok, Pid} = event_broker_sup:start_link(),
	eb_config:create(),
	start_feeds(application:get_env(event_broker, feeds, [])),
	{ok, Pid}.

stop(_State) ->
	eb_config:drop(),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

start_feeds([]) -> ok;
start_feeds([{Name, Filters}|T]) when is_binary(Name) andalso is_list(Filters) -> 
	event_broker:create_feed(Name, Filters),
	start_feeds(T);
start_feeds([H|T]) ->
	error_logger:error_msg("Invalid feed configuration: ~p\n", [H]),
	start_feeds(T).