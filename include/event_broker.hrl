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

-record(event_record, {name, date, ref, info}).

-define(EVENT(Name, Date, Ref, Info), #event_record{
		name=Name,
		date=Date,
		ref=Ref,
		info=Info
		}).

-define(NOTIFICATION(Event), {event, Event}).

-define(is_event(Name, Event), (is_record(Event, event_record) andalso Name =:= Event#event_record.name)).

-define(EB_FEED_CREATED, <<"event_broker:feed_created">>).
-define(EB_FEED_DROPPED, <<"event_broker:feed_dropped">>).