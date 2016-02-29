%%
%% Copyright 2016 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(eb_event_handler).

-include("event_broker.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-callback init(Args :: list()) -> 
	{ok, State :: term()} | {error, Reason :: term()}. 

-callback handle_event(Event :: #event_record{}, State :: term()) -> 
	{ok, NewState :: term()}. 

-callback handle_call(Request :: term(), State :: term()) 
	-> {ok, Reply :: term(), NewState :: term()} | {noreply, NewState :: term()}.

-callback handle_info(Info :: term(), State :: term()) 
	-> {ok, NewState :: term()}.

-callback terminate(State :: term()) 
	-> ok.

