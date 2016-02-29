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

-module(eb_window).

-include("event_broker.hrl").

-behaviour(eb_event_handler).

-define(HANDLER(Ref), {?MODULE, Ref}).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_time_window/5, start_length_window/5, stop_window/2]).

-spec start_time_window(Feed, Module, Args, WindowSize, UpdateInterval) -> Result when
	Feed :: atom(), 
	Module :: atom(), 
	Args :: term(), 
	WindowSize :: integer(), 
	UpdateInterval :: integer,
	Result :: {ok, Ref :: term()} 
	| {error, Reason :: term()}.
start_time_window(Feed, Module, Args, WindowSize, UpdateInterval) when is_atom(Feed) 
		andalso is_atom(Module) 
		andalso WindowSize > 0 
		andalso UpdateInterval > 0 ->
	Ref = make_ref(),
	case eb_feed:register(Feed, ?HANDLER(Ref), [Module, Args, time, WindowSize, UpdateInterval]) of
		ok -> {ok, Ref};
		Other -> Other
	end.

-spec start_length_window(Feed, Module, Args, WindowSize, UpdateInterval) -> Result when
	Feed :: atom(), 
	Module :: atom(), 
	Args :: list(), 
	WindowSize :: integer(), 
	UpdateInterval :: integer,
	Result :: {ok, Ref :: term()} 
	| {error, Reason :: term()}.
start_length_window(Feed, Module, Args, WindowSize, UpdateInterval) when is_atom(Feed) 
		andalso is_atom(Module) 
		andalso WindowSize > 0 
		andalso UpdateInterval > 0 ->
	Ref = make_ref(),
	case eb_feed:register(Feed, ?HANDLER(Ref), [Module, Args, length, WindowSize, UpdateInterval]) of
		ok -> {ok, Ref};
		Other -> Other
	end.

-spec stop_window(Feed :: atom(), Ref :: term()) -> ok | {error, Reason :: term()}.
stop_window(Feed, Ref) when is_atom(Feed) ->
	eb_feed:unregister(Feed, ?HANDLER(Ref)).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {module, type, size, timer, data, queue}).

%% init/1
init([Module, Args, Type, WindowSize, UpdateInterval]) ->
	case Module:init(Args) of
		{ok, Data} ->
			{ok, Timer} = timer:send_interval(UpdateInterval * 1000, {run_update}),
			State = #state{
					module=Module,
					type=Type,
					size=WindowSize,
					timer=Timer,
					data=Data,
					queue=queue:new()
					},
			{ok, State};
		{error, Reason} -> {error, Reason}
	end.

%% handle_event/2
handle_event(Event, State=#state{module=Module, data=Data, queue=Queue}) ->
	{Include, NewData} = try Module:filter(Event, Data)
	catch Error:Reason -> 
			LogArgs = [?MODULE, Module, Event, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:filter(~p, State) -> ~p:~p\n", LogArgs),
			{false, Data}
	end,	
	NewQueue = add_event(Include, Event, Queue),
	{ok, State#state{data=NewData, queue=NewQueue}}.

%% handle_call/2
handle_call(Request, State) ->
	error_logger:info_msg("~p(~p): Unexpected call message ~p\n", [?MODULE, self(), Request]),
	{noreply, State}.

%% handle_info/2
handle_info({run_update}, State=#state{module=Module, type=Type, size=Size, data=Data, queue=Queue}) ->
	NewQueue = purge(Type, Size, Queue),
	NewData = try Module:update(NewQueue, Data)
	catch Error:Reason -> 
			LogArgs = [?MODULE, Module, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:update(Queue, State) -> ~p:~p\n", LogArgs),
			Data
	end,
	{ok, State#state{data=NewData, queue=NewQueue}};
handle_info(Info, State) ->
	error_logger:info_msg("~p(~p): Unexpected message ~p\n", [?MODULE, self(), Info]),
	{ok, State}.

%% terminate/2
terminate(#state{timer=Timer}) ->
	timer:cancel(Timer),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

add_event(false, _Event, Queue) -> Queue;
add_event(true, Event, Queue) ->
	queue:in(Event, Queue).

purge(time, Size, Queue) ->
	Now = erlang:universaltime(),
	RefDate = calendar:datetime_to_gregorian_seconds(Now) - Size,
	queue:filter(fun(Event) ->
				EventDate = eb_event:get_date(Event),
				EventDateInSecs = calendar:datetime_to_gregorian_seconds(EventDate),
				EventDateInSecs >= RefDate
		end, Queue);
purge(length, Size, Queue) ->
	QueueSize = queue:len(Queue),
	case QueueSize =< Size of
		true -> Queue;
		false ->
			Ignore = QueueSize - Size,
			{_First, Second} = queue:split(Ignore, Queue),
			Second
	end.