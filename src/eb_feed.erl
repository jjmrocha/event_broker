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

-module(eb_feed).

-include("event_broker.hrl").

-define(SERVER(FeedName), {local, FeedName}).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
-export([register/3, unregister/2, list_handlers/1, is_registered/2]).
-export([publish/2, call/3]).

start_link(FeedName) ->
	gen_server:start_link(?SERVER(FeedName), ?MODULE, [FeedName], []).

-spec register(Feed :: atom(), Handler, Args :: list()) -> ok | {error, Reason :: term()} 
	when Handler :: Module | {Module, Ref},
	Module :: atom(),
	Ref :: term().
register(Feed, Handler, Args) when is_atom(Feed) ->
	gen_server:call(Feed, {add_handler, Handler, Args}).

-spec unregister(Feed :: atom(), Handler) -> ok | {error, Reason :: term()}
	when Handler :: Module | {Module, Ref},
	Module :: atom(),
	Ref :: term().
unregister(Feed, Handler) when is_atom(Feed) ->
	gen_server:call(Feed, {delete_handler, Handler}).

-spec list_handlers(Feed :: atom()) -> {ok, Handlers :: list()} | {error, Reason :: term()}.
list_handlers(Feed) when is_atom(Feed) ->
	gen_server:call(Feed, {list_handlers}).

-spec is_registered(Feed :: atom(), Handler) -> boolean()
	when Handler :: Module | {Module, Ref},
	Module :: atom(),
	Ref :: term().
is_registered(Feed, Handler) when is_atom(Feed) ->
	case gen_server:call(Feed, {get_handler_pid, Handler}) of
		{ok, _} -> true;
		_ -> false
	end.

-spec publish(Feed :: atom(), Event :: #event_record{}) -> ok.
publish(Feed, Event) when is_atom(Feed) andalso is_record(Event, event_record) ->
	gen_server:cast(Feed, {publish, Event}).	

-spec call(Feed :: atom(), Handler, Msg :: term()) -> term() 
	when Handler :: Module | {Module, Ref},
	Module :: atom(),
	Ref :: term().
call(Feed, Handler, Msg) when is_atom(Feed) ->
	case gen_server:call(Feed, {get_handler_pid, Handler}) of
		{ok, Pid} -> eb_handler:call(Pid, Msg);
		Other -> Other
	end.	

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {feed, handlers}).

%% init/1
init([FeedName]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("Feed ~p starting on [~p]...\n", [FeedName, self()]),
	{ok, #state{feed=FeedName, handlers=dict:new()}}.

%% handle_call/3
handle_call({get_handler_pid, Handler}, _From, State) ->
	Reply = case dict:find(Handler, State#state.handlers) of
		{ok, Pid} -> {ok, Pid};
		error -> {error, handler_not_found}
	end,
	{reply, Reply, State};
handle_call({add_handler, Handler, Args}, _From, State) ->
	{Reply, NewState} = case dict:find(Handler, State#state.handlers) of
		{ok, _} -> {{error, duplicated}, State};
		error ->
			{ok, Pid} = eb_handler_sup:create_handler(State#state.feed, Handler, Args),
			Handlers = dict:store(Handler, Pid, State#state.handlers),
			{ok, State#state{handlers=Handlers}}
	end,
	{reply, Reply, NewState};
handle_call({delete_handler, Handler}, _From, State) ->
	{Reply, NewState} = case dict:find(Handler, State#state.handlers) of
		{ok, Pid} ->
			eb_handler_sup:drop_handler(Pid),
			Handlers = dict:erase(Handler, State#state.handlers),
			{ok, State#state{handlers=Handlers}};
		error -> {{error, not_found}, State}
	end,
	{reply, Reply, NewState};
handle_call({list_handlers}, _From, State) ->
	Reply = dict:fetch_keys(State#state.handlers),
	{reply, Reply, State}.

%% handle_cast/2
handle_cast({publish, Event}, State) ->
	dict:fold(fun(_, Pid, _) ->
				eb_handler:event(Pid, Event)
		end, [], State#state.handlers),
	{noreply, State}.

%% handle_info/2
handle_info({update, Handler, Pid}, State) ->
	Handlers = dict:store(Handler, Pid, State#state.handlers),
	{noreply, State#state{handlers=Handlers}};
handle_info(_Info, State) ->
	{noreply, State}.

%% terminate/2
terminate(_Reason, State=#state{feed=FeedName}) ->
	lists:foreach(fun({_, Pid}) -> 
				eb_handler_sup:drop_handler(Pid)
		end, dict:to_list(State#state.handlers)),
	error_logger:info_msg("Feed ~p stopping...\n", [FeedName]),
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
