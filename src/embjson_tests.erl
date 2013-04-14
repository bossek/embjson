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

-module(embjson_tests).
-compile({parse_transform, embjson}).
-embjson([{callback, ?MODULE}, {function, '@json'}]).
-behaviour(embjson).

-export([object/1, array/1, string/1, number/1, boolean/1, null/1, other/1]).

-include_lib("eunit/include/eunit.hrl").

empty_top_level_object_test() -> ?assert(
    {struct, []}
    =:=
    '@json'({})
).

empty_top_level_array_test() -> ?assert(
    {array, []}
    =:=
    '@json'([])
).

static_object_test() -> ?assert(
    {struct, [
        {"string", "string value"},
        {"number", 12345},
        {"boolean", true},
        {"null", null},
        {"array", {array, ["a", "b", false, null]}},
        {"object", {struct, [
            {"boolean", false}
        ]}}
    ]}
    =:=
    '@json'({
        "string": "string value",
        "number": 12345,
        "boolean": true,
        "null": null,
        "array": ["a", "b", false, null],
        "object": {
            "boolean": false
        }
    })
).

variables_test() ->
    String = "string",
    Number = -123,
    ?assert(
        {struct, [
	    {"number", Number},
            {"string", String}
        ]}
        =:=
        '@json'({
	    "number": Number,
	    "string": String
        })
    ).

json_variable_test() ->
    Json = '@json'({"test": 123}),
    ?assert(
        {struct, [
            {"obj", {struct, [
                {"test", 123}
            ]}}
        ]}
        =:=
        '@json'({
            "obj": Json
        })
    ).

json_in_try_test() ->
    Json = try '@json'({"abc": "efg"}) catch _:_ -> bad end,
    ?assert({struct, [{"abc", "efg"}]} =:= Json).

%% ====================================================================
%% Callbacks generating JSON structure used in yaws.
%% @see https://github.com/klacke/yaws/blob/master/src/json2.erl
%% ====================================================================

object(Proplist) -> {struct, Proplist}.
array(List)      -> {array, List}.
string(String)   -> String.
number(Number)   -> Number.
boolean(Boolean) -> Boolean.
null(Null)       -> Null.
other(Value)     -> Value.
