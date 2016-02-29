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

-module(eb_handler).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).
-export([call/2, event/2]).

start_link(Feed, Handler, Args) ->
	gen_server:start_link(?MODULE, [Feed, Handler, Args], []).

call(Pid, Msg) ->
	gen_server:call(Pid, {call, Msg}).

event(Pid, Event) ->
	gen_server:cast(Pid, {event, Event}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {feed, module, handler, handler_state}).

%% init/1
init([Feed, Handler, Args]) ->
	Module = module(Handler),
	case Module:init(Args) of
		{ok, State} ->
			error_logger:info_msg("Handler ~p (~p) starting on [~p] for feed ~p...\n", [Handler, Module, self(), Feed]),
			Feed ! {update, Handler, self()},
			{ok, #state{feed=Feed, module=Module, handler=Handler, handler_state=State}};
		{error, Reason} ->
			{stop, Reason}
	end.

%% handle_call/3
handle_call({call, Msg}, _From, State=#state{module=Module, handler_state=HandlerState}) ->
	case Module:handle_call(Msg, HandlerState) of
		{ok, Reply, NewHandlerState} -> {reply, Reply, State#state{handler_state=NewHandlerState}};
		{noreply, NewHandlerState} ->{noreply, State#state{handler_state=NewHandlerState}}
	end.

%% handle_cast/2
handle_cast({event, Event}, State=#state{module=Module, handler_state=HandlerState}) ->
	{ok, NewHandlerState} = Module:handle_event(Event, HandlerState),
	{noreply, State#state{handler_state=NewHandlerState}}.

%% handle_info/2
handle_info(Info, State=#state{module=Module, handler_state=HandlerState}) ->
	{ok, NewHandlerState} = Module:handle_info(Info, HandlerState),
	{noreply, State#state{handler_state=NewHandlerState}}.

%% terminate/2
terminate(_Reason, #state{feed=Feed, module=Module, handler=Handler, handler_state=HandlerState}) ->
	Module:terminate(HandlerState),
	error_logger:info_msg("Handler ~p (~p) stopping for feed ~p...\n", [Handler, Module, Feed]),
	ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
module({Module, _}) -> Module;
module(Module) -> Module.

