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

-module(event_broker_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	EVENT_APP = {event_broker,{event_broker, start_link, []}, permanent, 2000, worker, [event_broker]},
	BROKER_SUP = {evb_broker_sup, {evb_broker_sup, start_link, []}, permanent, infinity, supervisor, [evb_broker_sup]},
	{ok, {{one_for_one, 5, 60}, [EVENT_APP, BROKER_SUP]}}.


