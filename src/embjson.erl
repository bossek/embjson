-module(embjson).

-export([parse_transform/2]).

-define(DEFAULT_FUNCTION, '@json').

-record(options, {callback, function}).

parse_transform(AST, _Options) ->
    EmbjsonOpts = embjson_opts(AST),
    % io:format("~p, ~p, ~p~n", [AST, Options, EmbjsonOpts]),
    TransModule = fun(AttrOrFun) -> trans_module(AttrOrFun, EmbjsonOpts) end,
    NewAST = lists:map(TransModule, AST),
    % io:format("~p~n", [NewAST]),
    NewAST.

embjson_opts(AST) ->
    embjson_opts(AST, undefined).

embjson_opts([{attribute, _, module, Module}|AST], _) ->
    embjson_opts(AST, Module);
embjson_opts([{attribute, _, embjson, Opts}|_], Module) ->
    Callback = proplists:get_value(callback, Opts, Module),
    Function = proplists:get_value(function, Opts, ?DEFAULT_FUNCTION),
    #options{callback = Callback, function = Function};
embjson_opts([], Module) ->
    #options{callback = Module, function = ?DEFAULT_FUNCTION};
embjson_opts([_|AST], Module) ->
    embjson_opts(AST, Module).

trans_module({function, Line, Name, ParamNum, Clauses}, Opts) ->
    {function, Line, Name, ParamNum, trans_clauses(Clauses, Opts)};
trans_module(AST, _Opts) ->
    % io:format("Other: ~p~n", [AST]),
    AST.

trans_clauses(Clauses, Opts) ->
    lists:map(fun(Clause) -> trans_clause(Clause, Opts) end, Clauses).

trans_clause({clause, Line, Vars, Guards, Exprs}, Opts) ->
    TransExpr = fun(AST) -> trans_expr(AST, Opts) end,
    {clause, Line, Vars, Guards, lists:map(TransExpr, Exprs)}.

trans_expr({call, _Line, {atom, _Line, Function}, [Param]}, Opts)
  when Function =:= Opts#options.function ->
    json(Param, Opts);
trans_expr({match, Line, Left, Right}, Opts) ->
    {match, Line, trans_expr(Left, Opts), trans_expr(Right, Opts)};
trans_expr({'case', Line, Expr, Clauses}, Opts) ->
    {'case', Line, trans_expr(Expr, Opts), trans_clauses(Clauses, Opts)};
trans_expr(AST, _Opts) ->
    % io:format("Other: ~p~n", [AST]),
    AST.

json({tuple, Line, _} = Object, Opts) ->
    callback(object, Line, object(Object, Opts), Opts);
json({nil, Line} = Array, Opts) ->
    callback(array, Line, array(Array, Opts), Opts);
json({cons, Line, _, _} = Array, Opts) ->
    callback(array, Line, array(Array, Opts), Opts).

object({tuple, Line, Props}, Opts) ->
    object(Props, Opts, Line).

object([], _Opts, LastLine) ->
    {nil, LastLine};
object([{remote, Line, Name, Value}|Props], Opts, _) ->
    {cons, Line, {tuple, Line, [Name, value(Value, Opts)]}, object(Props, Opts, Line)}.

array({nil, Line}, _Opts) ->
    {nil, Line};
array({cons, Line, Value, Tail}, Opts) ->
    {cons, Line, value(Value, Opts), array(Tail, Opts)}.

value({tuple, Line, _} = Object, Opts) ->
    callback(object, Line, object(Object, Opts), Opts);
value({nil, Line} = Array, Opts) ->
    callback(array, Line, array(Array, Opts), Opts);
value({cons, Line, _, _} = Array, Opts) ->
    callback(array, Line, array(Array, Opts), Opts);
value(Other, _Opts) ->
    Other.

callback(Function, Line, Param, Opts) ->
    {call, Line, {remote, Line, {atom, Line, Opts#options.callback}, {atom, Line, Function}}, [Param]}.
