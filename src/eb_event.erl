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

-module(eb_event).

-include("event_broker.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([name/2]).
-export([new/2, new/3, new/4]).
-export([get_name/1, get_date/1, get_ref/1, get_info/1]).
-export([get_property/2]).

name(Namespace, Name) when is_atom(Namespace) andalso is_binary(Name) ->
	BinNamespace = atom_to_binary(Namespace, utf8),
	name(BinNamespace, Name);
name(Namespace, Name) when is_binary(Namespace) andalso is_binary(Name) ->
	<<Namespace, $:, Name>>.

-spec new(Name::binary(), Ref::term()) -> #event_record{}.
new(Name, Ref) when is_binary(Name) ->
	Now = erlang:universaltime(),
	new(Name, Now, Ref, []).

-spec new(Name::binary(), Ref::term(), Info::list()) -> #event_record{}.
new(Name, Ref, Info) when is_binary(Name) andalso is_list(Info) ->
	Now = erlang:universaltime(),
	new(Name, Now, Ref, Info).

-spec new(Name::binary(), Date::calendar:datetime(), Ref::term(), Info::list()) -> #event_record{}.
new(Name, Date = {{_,_,_}, {_,_,_}}, Ref, Info) when is_binary(Name) andalso is_list(Info) ->
	#event_record{name=Name, date=Date, ref=Ref, info=Info}.

-spec get_name(Event::#event_record{}) -> binary().
get_name(Event) when is_record(Event, event_record) ->
	Event#event_record.name.

-spec get_date(Event::#event_record{}) -> calendar:datetime().
get_date(Event) when is_record(Event, event_record) ->
	Event#event_record.date.

-spec get_ref(Event::#event_record{}) -> term().
get_ref(Event) when is_record(Event, event_record) ->
	Event#event_record.ref.

-spec get_info(Event::#event_record{}) -> list().
get_info(Event) when is_record(Event, event_record) ->
	Event#event_record.info.

-spec get_property(Key::term(), Event::#event_record{}) -> {ok, Value::term()} | error.
get_property(Key, Event) when is_record(Event, event_record) ->
	case lists:keyfind(Key, 1, Event#event_record.info) of
		{_, Value} -> {ok, Value};
		false -> error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


