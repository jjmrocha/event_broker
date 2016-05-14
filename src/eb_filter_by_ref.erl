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

-module(eb_filter_by_ref).

-include("event_broker.hrl").

-behaviour(eb_filter_handler).

-export([init/1, filter/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_filter/2, stop_filter/2]).

-spec start_filter(Feed :: atom(), EventRef :: term()) -> {ok, Ref :: term()} | {error, Reason :: term()}.
start_filter(Feed, EventRef) when is_atom(Feed) ->
	eb_filter:register_filter(Feed, ?MODULE, [EventRef], EventRef).	

-spec stop_filter(Feed :: atom(), EventRef :: term()) -> ok | {error, Reason :: term()}.
stop_filter(Feed, EventRef) when is_atom(Feed) ->
	eb_filter:remove_filter(Feed, ?MODULE, EventRef).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([SearchRef]) -> {ok, SearchRef}.

filter(#event_record{ref=SearchRef}, SearchRef) -> {true, SearchRef};
filter(_Event, SearchRef) -> {false, SearchRef}.

%% ====================================================================
%% Internal functions
%% ====================================================================

