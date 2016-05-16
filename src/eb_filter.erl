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

-define(HANDLER(Module, Ref), {?MODULE, {Module, Ref}}).

-behaviour(eb_event_handler).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_filter/3, register_filter/4, remove_filter/3]).

-spec register_filter(Feed :: atom(), Module :: atom(), Args :: list()) -> {ok, Ref :: term()} | {error, Reason :: term()}.
register_filter(Feed, Module, Args) when is_atom(Feed) andalso is_atom(Module) andalso is_list(Args) ->
	Ref = make_ref(),
	register_filter(Feed, Module, Args, Ref).	

-spec register_filter(Feed :: atom(), Module :: atom(), Args :: list(),  Ref :: term()) -> {ok, Ref :: term()} | {error, Reason :: term()}.
register_filter(Feed, Module, Args, Ref) when is_atom(Feed) andalso is_atom(Module) andalso is_list(Args) ->
	Subscriber = self(),
	case eb_feed:register(Feed, ?HANDLER(Module, Ref), [Feed, Subscriber, Module, Args, Ref]) of
		ok -> {ok, Ref};
		Other -> Other
	end.

-spec remove_filter(Feed :: atom(), Module :: atom(), Ref :: term()) -> ok | {error, Reason :: term()}.
remove_filter(Feed, Module, Ref) when is_atom(Feed) andalso is_atom(Module) ->
	eb_feed:unregister(Feed, ?HANDLER(Module, Ref)).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {feed, pid, monitor, module, data, ref}).

%% init/1
init([Feed, Subscriber, Module, Args, Ref]) ->
	case Module:init(Args) of
		{ok, Data} -> 
			Monitor = erlang:monitor(process, Subscriber),
			State = #state{feed=Feed,
					pid=Subscriber,
					monitor=Monitor,
					module=Module,
					data=Data,
					ref=Ref},
			{ok, State};
		{error, Reason} ->
			{error, Reason}
	end.

%% handle_event/2
handle_event(Event, State=#state{pid=Subscriber, module=Module, data=Data}) ->
	{Send, NewData} = try Module:filter(Event, Data)
	catch Error:Reason -> 
			LogArgs = [?MODULE, Module, Event, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:filter(~p, State) -> ~p:~p\n", LogArgs),
			{false, Data}
	end,
	send(Send, Subscriber, Event),
	{ok, State#state{data=NewData}}.

%% handle_call/2
handle_call(Request, State) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, State}.

%% handle_info/2
handle_info({'DOWN', _, _, Subscriber, _}, State=#state{feed=Feed, pid=Subscriber, module=Module, ref=Ref}) ->
	eb_feed:unregister(Feed, ?HANDLER(Module, Ref)),
	{ok, State};
handle_info(Info, State) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{ok, State}.

%% terminate/2
terminate(#state{monitor=Monitor}) ->
	erlang:demonitor(Monitor),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

send(true, Subscriber, Event) ->
	Notification = ?NOTIFICATION(Event),
	Subscriber ! Notification;
send(false, _Subscriber, _Event) -> ok.
