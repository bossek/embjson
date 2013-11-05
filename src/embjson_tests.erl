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
-embjson([{callback, embjson_yaws_json2}, {function, '@json'}]).

-include_lib("eunit/include/eunit.hrl").

empty_top_level_object_test() -> ?assertEqual(
    {struct, []}
    ,
    '@json'({})
).

empty_top_level_array_test() -> ?assertEqual(
    {array, []}
    ,
    '@json'([])
).

static_object_test() -> ?assertEqual(
    {struct, [
        {"string", "string value"},
        {"number", 12345},
        {"neg_num", -3.14},
        {"boolean", true},
        {"null", null},
        {"array", {array, ["a", "b", false, null]}},
        {"object", {struct, [
            {"boolean", false}
        ]}}
    ]}
    ,
    '@json'({
        "string": "string value",
        "number": 12345,
        "neg_num": (-3.14),
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
    ?assertEqual(
        {struct, [
	    {"number", Number},
            {"string", String}
        ]}
        ,
        '@json'({
	    "number": Number,
	    "string": String
        })
    ).

json_variable_test() ->
    Json = '@json'({"test": 123}),
    ?assertEqual(
        {struct, [
            {"obj", {struct, [
                {"test", 123}
            ]}}
        ]}
        ,
        '@json'({
            "obj": Json
        })
    ).

json_in_try_test() ->
    Json = try '@json'({"abc": "efg"}) catch _:_ -> bad end,
    ?assertEqual({struct, [{"abc", "efg"}]}, Json).
