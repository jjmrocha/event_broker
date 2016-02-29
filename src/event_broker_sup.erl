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

-module(event_broker_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	{ok, Multiplier} = application:get_env(processes_by_core),
	WorkerCount = erlang:system_info(schedulers) * Multiplier,
	BROKER = {event_broker, {worker_pool_sup, start_pool, [event_broker, WorkerCount, {event_broker, start_link, []}]}, permanent, 2000, supervisor, [worker_pool_sup]},
	FEED_SUP = {eb_feed_sup, {eb_feed_sup, start_link, []}, permanent, 2000, supervisor, [eb_feed_sup]},
	HANDLER_SUP = {eb_handler_sup, {eb_handler_sup, start_link, []}, permanent, 2000, supervisor, [eb_handler_sup]},
	{ok, {{one_for_one, 5, 60}, [BROKER, FEED_SUP, HANDLER_SUP]}}.


