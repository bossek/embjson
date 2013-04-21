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
%%% @doc Embedded JSON in Erlang sources
%%%
%%% Parse transform library allowing embedding JSON directly into Erlang
%%% sources.
%%%
%%% @end
%%%

-module(embjson).

-export([parse_transform/2]).

%% ====================================================================
%% embjson behaviour
%% ====================================================================

-callback property(Name :: string()) -> term().
-callback object(Proplist :: proplists:proplist()) -> term().
-callback array(List :: list()) -> term().
-callback string(String :: string()) -> term().
-callback number(Number :: number()) -> term().
-callback boolean(Boolean :: boolean()) -> term().
-callback null(Null :: 'null') -> term().
-callback other(Other :: term()) -> term().

%% ====================================================================
%% transformation
%% ====================================================================

-define(DEFAULT_FUNCTION, '@json').

-record(options, {callback, function = ?DEFAULT_FUNCTION, file}).

parse_transform(AST, _Options) ->
    Tree = erl_syntax:form_list(AST),
    Opts = embjson_opts(Tree),
    erl_syntax:revert_forms(erl_syntax_lib:map(embjson(Opts), Tree)).

%% @doc Finds options (proplist) in 'embjson' attribute.
%% Keys:
%%     callback
%%         - Module with callback functions. If callback module is not
%%           configured, embjson uses transformed module.
%%     function
%%         - Name of a function used for encapsulating of embedded JSON.
%%           If function is not defined, embjson uses '@json'.
%% @end
embjson_opts(Tree) ->
    erl_syntax_lib:fold(fun find_options/2, #options{}, Tree).

find_options(Node, Options) ->
    case erl_syntax:type(Node) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Node) of
                {embjson, Info} ->
                    fill_options(Info, Options);
                {module, Module} ->
                    fill_callback(Module, Options);
                {file, {Name, _}} ->
                    Options#options{file = Name};
                _ ->
                    Options
            end;
        _ ->
            Options
    end.

fill_options({embjson, Props}, Options) ->
    Callback = proplists:get_value(callback, Props, Options#options.callback),
    Function = proplists:get_value(function, Props, ?DEFAULT_FUNCTION),
    #options{callback = Callback, function = Function}.

fill_callback(Module, Options) ->
    case Options#options.callback of
        undefined ->
            Options#options{callback = Module};
        _ ->
            Options
    end.

embjson(Opts) ->
    fun(Node) ->
        case erl_syntax:type(Node) of
            application ->
                case erl_syntax_lib:analyze_application(Node) of
                    {Name, 1} when Name =:= Opts#options.function ->
                        F = fun(Tree, Acc) -> json(Tree, Acc, Opts) end,
                        erl_syntax_lib:fold_subtrees(F, undefined, Node);
                    _ ->
                        Node
                end;
            _ ->
                Node
        end
    end.

json(Tree, Result, Opts) ->
    case erl_syntax:type(Tree) of
        atom ->
            Value = erl_syntax:atom_value(Tree),
            Value = Opts#options.function,
            Result;
        nil ->
            callback(array, array(Tree, Opts), Opts);
        list ->
            callback(array, array(Tree, Opts), Opts);
        tuple ->
            callback(object, object(Tree, Opts), Opts);
        _ ->
            embjson_error(embjson_invalid_json, Tree, Opts)
    end.

object(Tree, Opts) ->
    Object = [pair(E, Opts) || E <- erl_syntax:tuple_elements(Tree)],
    erl_syntax:copy_attrs(Tree, erl_syntax:list(Object)).

pair(Tree, Opts) ->
    case erl_syntax:type(Tree) of
        module_qualifier ->
            Name = erl_syntax:module_qualifier_argument(Tree),
            case erl_syntax:type(Name) of
                string ->
                    P = callback(property, Name, Opts),
                    V = value(erl_syntax:module_qualifier_body(Tree), Opts),
                    erl_syntax:copy_attrs(Tree, erl_syntax:tuple([P, V]));
                _ ->
                    embjson_error(embjson_invalid_property_name, Tree, Opts)
            end;
        _ ->
            embjson_error(embjson_invalid_object_property, Tree, Opts)
    end.

array(Tree, Opts) ->
    case erl_syntax:type(Tree) of
        nil ->
            Tree;
        list ->
            List = [value(E, Opts) || E <- erl_syntax:list_elements(Tree)],
            erl_syntax:copy_attrs(Tree, erl_syntax:list(List))
    end.

value(Tree, Opts) ->
    {F, T} = case erl_syntax:type(Tree) of
                 prefix_expr -> {get_prefix_expr_callback(Tree), Tree};
                 integer     -> {number, Tree};
                 float       -> {number, Tree};
                 string      -> {string, Tree};
                 atom        -> {atom(Tree, Opts), Tree};
                 tuple       -> {object, object(Tree, Opts)};
                 nil         -> {array, array(Tree, Opts)};
                 list        -> {array, array(Tree, Opts)};
                 _           -> {other, Tree}
             end,
    callback(F, T, Opts).

get_prefix_expr_callback(Tree) ->
    case erl_syntax:type(Tree) of
        prefix_expr -> get_prefix_expr_callback(erl_syntax:prefix_expr_argument(Tree));
        integer     -> number;
        float       -> number;
        _           -> other
    end.

atom(Tree, Opts) ->
    case erl_syntax:atom_value(Tree) of
        true  -> boolean;
        false -> boolean;
        null  -> null;
        _     -> embjson_error(embjson_invalid_value, Tree, Opts)
    end.

callback(Function, Param, Opts) ->
    M = erl_syntax:atom(Opts#options.callback),
    F = erl_syntax:atom(Function),
    A = erl_syntax:application(M, F, [Param]),
    erl_syntax:set_pos(A, erl_syntax:get_pos(Param)).

embjson_error(Error, Tree, Opts) ->
    Detail = [{file, Opts#options.file}, {line, erl_syntax:get_pos(Tree)}],
    error({Error, Detail}).
