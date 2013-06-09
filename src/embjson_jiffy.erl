%%%
%%% Copyright 2013 Lubos Vesely
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author bossek <lv.bossek@gmail.com>
%%%
%%% @doc Callbacks generating JSON structure used in jiffy
%%% ([https://github.com/davisp/jiffy]).
%%%

-module(embjson_jiffy).
-behaviour(embjson).

-export([property/1, object/1, array/1, string/1, number/1, boolean/1, null/1, other/1]).

%% ====================================================================
%% 'embjson' behaviour callbacks
%% ====================================================================

property(Name)   -> unicode:characters_to_binary(Name).
object(Proplist) -> {Proplist}.
array(List)      -> List.
string(String)   -> unicode:characters_to_binary(String).
number(Number)   -> Number.
boolean(Boolean) -> Boolean.
null(Null)       -> Null.
other(Value)     -> Value.
