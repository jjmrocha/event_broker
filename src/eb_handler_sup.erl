%%
%% Copyright 2016 Joaquim Rocha
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

-module(eb_handler_sup).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([create_handler/3, drop_handler/1]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
	supervisor:start_link(?SERVER, ?MODULE, []).

create_handler(Feed, Handler, Args) ->
	supervisor:start_child(?MODULE, [Feed, Handler, Args]).

drop_handler(Pid) ->
	supervisor:terminate_child(?MODULE, Pid).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	error_logger:info_msg("~p [~p] Starting...\n", [?MODULE, self()]),
	Handler = {eb_handler, {eb_handler, start_link, []}, permanent, 2000, worker, [eb_handler]},
	{ok,{{simple_one_for_one, 10, 60}, [Handler]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================