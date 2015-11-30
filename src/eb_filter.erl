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

-module(eb_filter).

-include("event_broker.hrl").

-define(HANDLER(Pid), {?MODULE, Pid}).
-define(CONFIG(Feed, Pid, Module), {Feed, Pid, Module}).

-callback filter(Event :: #event_record{}) -> boolean(). 

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([subscribe/2, unsubscribe/1]).

-spec subscribe(Feed :: binary(), Module :: atom()) -> ok | {error, Reason :: term()}.
subscribe(Feed, Module) when is_binary(Feed) andalso is_atom(Module) ->
	Subscriber = self(),
	eb_feed:register(Feed, ?HANDLER(Subscriber), ?CONFIG(Feed, Subscriber, Module)).	

-spec unsubscribe(Feed :: binary()) -> ok | {error, Reason :: term()}.
unsubscribe(Feed) ->
	Subscriber = self(),
	eb_feed:unregister(Feed, ?HANDLER(Subscriber)).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([Config = ?CONFIG(_Feed, Subscriber, _Module)]) ->
	erlang:monitor(process, Subscriber),
	{ok, Config}.

%% handle_event/2
handle_event(Event, Config = ?CONFIG(_Feed, Subscriber, Module)) ->
	try Module:filter(Event) of
		true -> 
			Notification = ?NOTIFICATION(Event),
			Subscriber ! Notification;
		fale -> ok
	catch Error:Reason -> 
			LogArgs = [?MODULE, Module, Event, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:filter(~p) -> ~p:~p\n", LogArgs)
	end,
	{ok, Config}.

%% handle_call/2
handle_call(Request, Config) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, Config}.

%% handle_info/2
handle_info({'DOWN', _, _, Subscriber, _}, Config = ?CONFIG(Feed, Subscriber, _Module)) ->
	eb_feed:unregister(Feed, ?HANDLER(Subscriber)),
	{ok, Config};
handle_info(Info, Config) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{ok, Config}.

%% terminate/2
terminate(_Arg, _Config) ->
	ok.

%% code_change/3
code_change(_OldVsn, Config, _Extra) ->
	{ok, Config}.

%% ====================================================================
%% Internal functions
%% ====================================================================


