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

-module(eb_event).

-include("event_broker.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/2, new/3, new/4]).
-export([name/1, date/1, ref/1, info/1]).
-export([property/2]).

-spec new(Name::binary(), Ref::term()) -> #event_record{}.
new(Name, Ref) when is_binary(Name) ->
	Now = erlang:universaltime(),
	new(Name, Now, Ref, maps:new()).

-spec new(Name::binary(), Date::calendar:datetime(), Ref::term()) -> #event_record{}.
new(Name, Date = {{_,_,_}, {_,_,_}}, Ref) when is_binary(Name) ->
	new(Name, Date, Ref, maps:new()).

-spec new(Name::binary(), Date::calendar:datetime(), Ref::term(), Info::map()) -> #event_record{}.
new(Name, Date = {{_,_,_}, {_,_,_}}, Ref, Info) when is_binary(Name) andalso is_map(Info) ->
	#event_record{name=Name, date=Date, ref=Ref, info=Info}.

-spec name(Event::#event_record{}) -> binary().
name(Event) when is_record(Event, event_record) ->
	Event#event_record.name.

-spec date(Event::#event_record{}) -> calendar:datetime().
date(Event) when is_record(Event, event_record) ->
	Event#event_record.date.

-spec ref(Event::#event_record{}) -> term().
ref(Event) when is_record(Event, event_record) ->
	Event#event_record.ref.

-spec info(Event::#event_record{}) -> map().
info(Event) when is_record(Event, event_record) ->
	Event#event_record.info.

-spec property(Key::term(), Event::#event_record{}) -> {ok, Value::term()} | error.
property(Key, Event) when is_record(Event, event_record) ->
	maps:find(Key, Event#event_record.info).

%% ====================================================================
%% Internal functions
%% ====================================================================


