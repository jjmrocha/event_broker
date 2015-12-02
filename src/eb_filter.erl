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

-callback init(Args :: term()) -> {ok, State :: term()} | {error, Reason :: term()}. 
-callback filter(Event :: #event_record{}, State :: term()) -> {Response :: boolean(), NewState :: term()}. 

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_filter/3, remove_filter/1]).

-spec register_filter(Feed :: binary(), Module :: atom(), Args :: term()) -> ok | {error, Reason :: term()}.
register_filter(Feed, Module, Args) when is_binary(Feed) andalso is_atom(Module) ->
	Subscriber = self(),
	eb_feed:register(Feed, ?HANDLER(Subscriber), [Feed, Subscriber, Module, Args]).	

-spec remove_filter(Feed :: binary()) -> ok | {error, Reason :: term()}.
remove_filter(Feed) ->
	Subscriber = self(),
	eb_feed:unregister(Feed, ?HANDLER(Subscriber)).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {feed, pid, monitor, module, data}).

%% init/1
init([Feed, Subscriber, Module, Args]) ->
	case Module:init(Args) of
		{ok, Data} -> 
			Ref = erlang:monitor(process, Subscriber),
			State = #state{feed=Feed,
					pid=Subscriber,
					monitor=Ref,
					module=Module,
					data=Data},
			{ok, State};
		{error, Reason} -> {error, Reason}
	end.

%% handle_event/2
handle_event(Event, State=#state{pid=Subscriber, module=Module, data=Data}) ->
	{Send, NewData} = try Module:filter(Event, Data)
	catch Error:Reason -> 
			LogArgs = [?MODULE, Module, Event, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:filter(~p) -> ~p:~p\n", LogArgs),
			{false, Data}
	end,
	send(Send, Subscriber, Event),
	{ok, State#state{data=NewData}}.

%% handle_call/2
handle_call(Request, State) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, State}.

%% handle_info/2
handle_info({'DOWN', _, _, Subscriber, _}, State=#state{feed=Feed, pid=Subscriber}) ->
	eb_feed:unregister(Feed, ?HANDLER(Subscriber)),
	{ok, State};
handle_info(Info, State) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{ok, State}.

%% terminate/2
terminate(_Arg, #state{monitor=Ref}) ->
	erlang:demonitor(Ref),
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

send(true, Subscriber, Event) ->
	Notification = ?NOTIFICATION(Event),
	Subscriber ! Notification;
send(false, _Subscriber, _Event) -> ok.
