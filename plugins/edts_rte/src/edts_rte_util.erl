%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This module contains the utility functions for edts rte

%%%_* Module declaration =======================================================
-module(edts_rte_util).

%%%_* Exports =================================================================
-export([ convert_list_to_term/1
        , extract_fun_clauses_line_num/1
        , get_function_abscode/3
        , is_tail_recursion/3
        , traverse_clause_struct/2
        , replace_value_in_fun/3
        ]).

%%%_* Includes =================================================================
-include_lib("kernel/include/file.hrl").

-record(clause_struct, { line       = undefined :: integer()
                       , sub_clause = undefined :: #clause_struct{}
                       , touched    = false     :: boolean()
                       }).

-type clause_struct() :: #clause_struct{}.

%%%_* API ======================================================================
%%------------------------------------------------------------------------------
%% @doc convert the argument list in the string format into the erlang term
-spec convert_list_to_term(Arguments :: string()) -> [any()].
convert_list_to_term(Arguments) ->
  edts_rte_app:debug("args:~p~n", [Arguments]),

  %% N.B. this is very hackish. added a '.' because
  %%      erl_parse:parse_exprs/1 requires full expression with dot
  {ok, Tokens,__Endline} = erl_scan:string(Arguments++"."),
  edts_rte_app:debug("args tokens:~p~n", [Tokens]),

  {ok, AbsForm0}         = erl_parse:parse_exprs(Tokens),
  edts_rte_app:debug("args form:~p~n", [AbsForm0]),

  AbsForm                = replace_value_in_expr(AbsForm0, [], [], true),
  edts_rte_app:debug("args replaced form:~p~n", [AbsForm]),

  {value, Value, _Bs}    = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  edts_rte_app:debug("args evaluated:~p~n", [Value]),
  Value.

%%------------------------------------------------------------------------------
%% @doc Extract all the line numbers of all the clauses from an abstract form
%%      for both anonymous function and normal function
-type line_no()   :: non_neg_integer().
-type clause_form() :: {clause, line_no(), [any()], [any()], [any()]}.
-type func_form() :: {'fun', line_no(), {clauses, [clause_form()]}}
                   | {function, line_no(), atom(), arity(), [clause_form()]}.
-spec extract_fun_clauses_line_num(FunForm :: func_form()) -> [clause_struct()].
extract_fun_clauses_line_num({'fun', _L, {clauses, Clauses}}) ->
  extract_clauses_line_num(Clauses);
extract_fun_clauses_line_num({function, _L, _Func, _Arity, Clauses}) ->
  extract_clauses_line_num(Clauses).

%%------------------------------------------------------------------------------
%% @doc get the abstract code of a function. support anonymous
%%      function as well
-spec get_function_abscode(module(), function(), arity()) ->
                              {ok, func_form()} | {error, any()}.
get_function_abscode(Module, Function, Arity) ->
  case is_lambda(Function) of
    false ->
      edts_code:get_function_abscode(Module, Function, Arity);
    {true, FatherFun, FatherArity, DefSeq}  ->
      get_lambda_abscode(Module, FatherFun, FatherArity, DefSeq, Function)
  end.

%%------------------------------------------------------------------------------
%% @doc When MFA and depth are the same, check if it is still a
%%      differnet function call.
%%      This could happen when:
%%      1) Tail recursion
%%      2) When function with the same name are called within the
%%         same expression.
%%         e.g.
%%         fib(N) ->
%%           fib(N-1) + fib(N-2)
%%
%%         In the example above, fib(N-2) will be called immediately
%%         after fib(N-1) is returned, making them have the same MFA
%%         and Depth.
%%
%%      To check this, we need to see if the the new line is either in
%%      the other clause of the same function or it is in the same clause
%%      but the new line is equal or smaller than the previous line.
-spec is_tail_recursion([clause_struct()], non_neg_integer(), non_neg_integer())
                       -> boolean().
is_tail_recursion(ClauseStructs, PreviousLine, NewLine) ->
  edts_rte_app:debug("8) is_tail_call:~p~n"
            , [[ClauseStructs, PreviousLine, NewLine]]),
  {LineSmallerClauses, _LineBiggerClauses} =
    lists:splitwith(fun(#clause_struct{line = L}) ->
                      L =< NewLine
                    end, ClauseStructs),
  edts_rte_app:debug("9) LineSmaller:~p~nLineBigger:~p~n"
            , [LineSmallerClauses, _LineBiggerClauses]),
  #clause_struct{touched = Touched, line = L} =
    hd(lists:reverse(LineSmallerClauses)),
  case Touched of
    false -> true;
    true  ->
      %% assert
      true = PreviousLine >= L,
      %% if previous line is bigger or equal than the new line, then it
      %% should be a tail recursion
      PreviousLine >= NewLine
  end.

%%------------------------------------------------------------------------------
%% @doc traverse all the clauses and mark all the touched node
%%      if one of the clause in a group of clauses are touched,
%%      do not touch the rest of the clause.
traverse_clause_struct(_Line, []) ->
  [];
traverse_clause_struct(Line, [H|_T] = ClausesGroups) when is_list(H) ->
  SmallerLnF = fun(ClauseStructs) ->
                   ClauseStruct = hd(ClauseStructs),
                   ClauseStruct#clause_struct.line =< Line
               end,
  {SmallerLnClausesGroups, BiggerOrEqualLnClausesGroups} =
    lists:splitwith(SmallerLnF, ClausesGroups),
  do_traverse_clause_group(SmallerLnClausesGroups, Line) ++
    BiggerOrEqualLnClausesGroups;
traverse_clause_struct(Line, ClauseStructs) ->
  Touched =
    lists:any(fun(ClauseStruct) ->
                  ClauseStruct#clause_struct.touched
              end, ClauseStructs),
  SmallerLnF = fun(ClauseStruct) ->
                   ClauseStruct#clause_struct.line =< Line
               end,
  {SmallerLnClauses, BiggerOrEqualLnClauses} =
    lists:splitwith(SmallerLnF, ClauseStructs),
  do_traverse_clause_struct(SmallerLnClauses, Line, Touched) ++
    BiggerOrEqualLnClauses.

%%------------------------------------------------------------------------------
%% @doc replace the temporary variables with the actual value in a function
-spec replace_value_in_fun( FunBody       :: string()
                          , AllClausesLn  :: #clause_struct{}
                          , Bindings      :: edts_rte_server:bindings())
                          -> string().
replace_value_in_fun(AbsForm, AllClausesLn, Bindings) ->
  %% Replace variable names with variables' value and
  %% combine the Token to function string again
  NewFunBody            = do_var_to_val_in_fun( AbsForm
                                              , AllClausesLn
                                              , Bindings),
  %% edts_rte_app:debug("New Body before flatten: ~p~n", [NewFunBody]),
  NewForm               = erl_pp:form(NewFunBody),
  lists:flatten(NewForm).

%%%_* Internal =================================================================
%%------------------------------------------------------------------------------
%% @doc Extract all the line numbers of a list of clauses in the abstract form
-spec extract_clauses_line_num([clause_form()]) -> [clause_struct()].
extract_clauses_line_num([]) ->
  [];
extract_clauses_line_num([{clause,L,_ArgList0,_WhenList0,Exprs0}|T]) ->
  ExprsLn = extract_exprs_line_num(Exprs0),
  [ #clause_struct{line = L, sub_clause = ExprsLn}
  | extract_clauses_line_num(T)].

%%------------------------------------------------------------------------------
%% @doc Extract all the line numbers of the clauses from a list of expressions
%%      in the abstract form
extract_exprs_line_num(Exprs) ->
  lists:foldl(fun(Expr, LineNums) ->
                case extract_expr_line_num(Expr) of
                  []  -> LineNums;
                  Lns -> lists:reverse([Lns|LineNums])
                end
              end, [], Exprs).

%%------------------------------------------------------------------------------
%% @doc Extract all the line numbers of the clauses from an expression in the
%%      abstract form
extract_expr_line_num(Exprs) when is_list(Exprs)         ->
  extract_exprs(Exprs);
extract_expr_line_num({cons, _L, Expr, Rest})            ->
  extract_expr(Rest) ++ extract_expr(Expr);
extract_expr_line_num({tuple, _L, Exprs})                ->
  extract_exprs(Exprs);
extract_expr_line_num({match,_L,LExpr,RExpr})            ->
  extract_expr(RExpr) ++ extract_expr(LExpr);
extract_expr_line_num({op, _L, _Ops, Expr})              ->
  extract_expr(Expr);
extract_expr_line_num({op, _L, _Ops, LExpr, RExpr})      ->
  extract_expr(RExpr) ++ extract_expr(LExpr);
extract_expr_line_num({lc, _L, Expr, GenExprs})          ->
  extract_exprs(GenExprs) ++ extract_expr(Expr);
extract_expr_line_num({generate, _L, ResExp, GenExp})    ->
  extract_expr(GenExp) ++ extract_expr(ResExp);
extract_expr_line_num({'case', _L, CaseExpr, Clauses})   ->
  extract_clause(Clauses) ++ extract_expr(CaseExpr);
extract_expr_line_num({ 'try', _L, Exprs, PatternClauses
                      , ExceptionClauses, FinalExprs})   ->
  extract_expr(FinalExprs) ++ extract_clause(ExceptionClauses) ++
    extract_clause(PatternClauses) ++ extract_expr(Exprs);
extract_expr_line_num({'receive', _L, Clauses})          ->
  extract_clause(Clauses);
extract_expr_line_num(_)                                 ->
  [].

%% @doc Extract all the line numbers of the clauses from a list of expressions
%%      in the abstract form. Result in the reverse order of Exprs
extract_exprs(Exprs) ->
  lists:foldl( fun(Expr, NewExprs) -> extract_expr(Expr) ++ NewExprs end
             , [], Exprs).

extract_expr(Expr) ->
  extract(Expr, fun extract_expr_line_num/1).

extract_clause(Clauses) ->
  extract(Clauses, fun extract_clauses_line_num/1).

extract(Expr, F) ->
  case F(Expr) of
    L when is_list(L) -> L;
    E                 -> [E]
  end.

%%------------------------------------------------------------------------------
%% @doc if an atom is the name of an anonymous function. the format is like
%%      the following:
%%      "-Funname/Arity-fun-N-"
%%      where Funname is the name of the function, Arity is the number of
%%      the arguments the function would take and N is the sequence of the
%%      anonymous function defined in Funname.
-spec is_lambda(Function :: atom()) ->
                   {true, function(), arity(), integer()} | false.
is_lambda(Function) ->
  FuncStr = atom_to_list(Function),
  case re:run(FuncStr, "-(.*)/(.*)-fun-(.*)-", [{capture, [1,2,3], list}]) of
    {match, [FatherFunStr, FatherArityStr, DefinedSeq]} ->
      {true, list_to_atom(FatherFunStr),
             list_to_integer(FatherArityStr),
             list_to_integer(DefinedSeq)};
    nomatch ->
      false
  end.

%%------------------------------------------------------------------------------
%% @doc get the abstract code for an lambda function.
-spec get_lambda_abscode( module(), function(), arity()
                        , non_neg_integer(), function()) -> {ok, func_form()}.
get_lambda_abscode(Module, FatherFun, FatherArity, DefSeq, Function) ->
  {ok, FunAbsForm} = edts_code:get_function_abscode( Module
                                                   , FatherFun, FatherArity),
  {ok, normalize_fun( lists:nth( DefSeq+1
                               , extract_lambda_abscode_from_fun(FunAbsForm))
                    , Function)}.

%%------------------------------------------------------------------------------
%% @doc extract the abstract code for an lambda function from its parent
%%      function abstract code.
extract_lambda_abscode_from_fun({function, _L, _Name, _Arity, Clauses}) ->
  lists:sort(fun( {'fun', L1, {clauses, _Clauses1}}
                , {'fun', L2, {clauses, _Clauses2}}) ->
                 L2 > L1
             end, extract_lambda_abscode_from_clauses(Clauses)).

%%------------------------------------------------------------------------------
%% @doc extract the abstract code for all the lambda function in a list
%%      of the clauses, in the order of their definition.
extract_lambda_abscode_from_clauses(Clauses) ->
  lists:foldl(fun(Clause, Acc) ->
                extract_lambda_abscode_from_clause(Clause) ++ Acc
              end, [], Clauses).

%%------------------------------------------------------------------------------
%% @doc extract the abstract code for a lambda function from a clause
extract_lambda_abscode_from_clause({clause, _L, _ArgList, _WhenList, Exprs}) ->
  lists:foldl(fun(Expr, Acc) ->
                extract_lambda_abscode_from_expr(Expr) ++ Acc
              end, [], Exprs).

%%------------------------------------------------------------------------------
%% @doc extract the abstract code for a lambda function from an expression
%% TODO: Need to extract the labmda abscode from more expressions.
%%       Really want a simpler way way of handling this.
extract_lambda_abscode_from_expr({'case', _L, CaseExpr, Clauses}) ->
  extract_lambda_abscode_from_expr(CaseExpr) ++
    extract_lambda_abscode_from_clauses(Clauses);
extract_lambda_abscode_from_expr({match, _L, _LExpr0, RExpr0}) ->
  extract_lambda_abscode_from_expr(RExpr0);
extract_lambda_abscode_from_expr({call, _L1, {remote, _L2, _M, _F}, Args}) ->
  lists:foldl(fun(Arg, Acc) ->
                extract_lambda_abscode_from_expr(Arg) ++ Acc
              end, [], Args);
extract_lambda_abscode_from_expr({'fun', _L, {clauses, Clauses}} = AbsCode) ->
  [AbsCode|extract_lambda_abscode_from_clauses(Clauses)];
extract_lambda_abscode_from_expr(_) ->
  [].

do_traverse_clause_group([], _Line) ->
  [];
do_traverse_clause_group(SmallerClassesGroup, Line) ->
  Reversed = lists:reverse(SmallerClassesGroup),
  lists:reverse([traverse_clause_struct(Line, hd(Reversed))|tl(Reversed)]).

do_traverse_clause_struct([], _line, _Touched) ->
  [];
do_traverse_clause_struct(SmallerLnClauses, Line, Touched) ->
  [ClauseStruct|T] = lists:reverse(SmallerLnClauses),
  %% check if other clauses in the same group has been touched.
  case Touched of
    true  -> case ClauseStruct#clause_struct.touched of
               true  -> lists:reverse([touch_clause(ClauseStruct, Line)|T]);
               false -> SmallerLnClauses
             end;
    false -> lists:reverse([touch_clause(ClauseStruct, Line)|T])
  end.

touch_clause(ClauseStruct, Line) ->
  SubClauseStruct0 = ClauseStruct#clause_struct.sub_clause,
  SubClauseStruct  = traverse_clause_struct(Line, SubClauseStruct0),
  ClauseStruct#clause_struct{ touched = true
                            , sub_clause = SubClauseStruct}.

%%------------------------------------------------------------------------------
%% @doc replace variable names with values for a function
do_var_to_val_in_fun( {'fun', L, {clauses, Clauses0}}
                    , AllClausesLn, Bindings) ->
  Clauses = replace_value_in_clauses(Clauses0, AllClausesLn, Bindings),
  {'fun', L, {clauses, Clauses}};
do_var_to_val_in_fun( {function, L, FuncName, Arity, Clauses0}
                    , AllClausesLn, Bindings) ->
  Clauses = replace_value_in_clauses(Clauses0, AllClausesLn, Bindings),
  {function, L, FuncName, Arity, Clauses}.

%%------------------------------------------------------------------------------
%% @doc replace variable names with values in each of the clauses for
%%      anonymous functions.
%%      there are two differences between this function and @see
%%      replace_value_in_clauses.
%%      1) We don't need to care about if the function clause is executed
%%         or not. because this is just the definition of the anonymous
%%         function and it is not being executed yet.
%%      2) We need to consider the shadowing. e.g. if A is in the arglist
%%         of a function clause in the anonymous function, we can't replace
%%         the temporary variable A even if A is already defined from outside.
replace_value_in_lambda_clauses(Clauses, AllClausesLn, Binding0, ExpandRcd) ->
  lists:map(fun({clause,_L,ArgList,_WhenList0,_Lines0} = Clause) ->
                Vars    = extract_vars(ArgList),
                Binding = lists:foldl(fun(Var, TmpBinding) ->
                                        lists:keydelete(Var, 1, TmpBinding)
                                      end, Binding0, Vars),
                do_replace_value_in_clause( Clause, AllClausesLn
                                          , Binding, ExpandRcd)
            end, Clauses).

extract_vars(ArgList) ->
  lists:foldl(fun(Arg, Acc) ->
                extract_var(Arg) ++ Acc
              end, [], ArgList).

%% This needs to be extended to support more programming constructs
extract_var({var, _, VarName}) ->
  [VarName];
extract_var(_) ->
  [].

replace_value_in_clauses(Clauses, AllClausesLn, Binding) ->
  replace_value_in_clauses(Clauses, AllClausesLn, Binding, false).

%%------------------------------------------------------------------------------
%% @doc replace variable names with values in each of the clauses
replace_value_in_clauses(Clauses, AllClausesLn, Binding, ExpandRecord) ->
  lists:map(fun({clause,L,_ArgList0,_WhenList0,_Lines0} = Clause) ->
                case is_clause_touched(L, AllClausesLn) of
                  true  -> do_replace_value_in_clause( Clause, AllClausesLn
                                                     , Binding, ExpandRecord);
                  false -> Clause
                end
            end, Clauses).

is_clause_touched(_L, [])                                  ->
  false;
is_clause_touched(L, [H|T]) when is_list(H)                ->
  is_clause_touched(L, H) orelse is_clause_touched(L, T);
is_clause_touched(L, [H|_T]) when H#clause_struct.line > L ->
  false;
is_clause_touched(L, [H|T])                                ->
  (H#clause_struct.line =:= L andalso H#clause_struct.touched)
    orelse is_clause_touched(L, H#clause_struct.sub_clause)
    orelse is_clause_touched(L, T).

do_replace_value_in_clause( {clause,L,ArgList0,WhenList0,Lines0}
                          , AllClausesLn, Binding, ExpandRcd) ->
  %% replace all the variables with values for all the exprs in argument list
  ArgList  = replace_value_in_expr(ArgList0, AllClausesLn, Binding, ExpandRcd),
  %% replace variables' name with values in "when" list
  WhenList = replace_value_args(WhenList0, Binding),
  %% replace variables' name with values for each of the expressions
  Lines    = replace_value_in_expr(Lines0, AllClausesLn, Binding, ExpandRcd),
  {clause,L,ArgList,WhenList,Lines}.

replace_value_args([], _Bindings)         ->
  [];
replace_value_args([VarExpr|T], Bindings) ->
  [replace_value(VarExpr, Bindings) | replace_value_args(T, Bindings)].

%% @doc replace the variable in a list of expressions with its actual valuex
replace_value_in_exprs(Exprs, ExecClausesLn, Bindings, ExpandRcd) ->
  lists:map(fun(Expr) ->
              replace_value_in_expr(Expr, ExecClausesLn, Bindings, ExpandRcd)
            end, Exprs).

%%------------------------------------------------------------------------------
%% @doc replace the variable in the expression with its actual value
%%      it takes two extra parameters. the line number of all the clauses
%%      that were executed and the binding information.
%%      last parameter indicates if we want to expand the record or not
%%      normally we don't but when it is the input arguments we should.
replace_value_in_expr([], _ECLn, _Bs, _Er)                                   ->
  [];
replace_value_in_expr(Atom, _ECLn, _Bs, _Er) when is_atom(Atom)              ->
  Atom;
replace_value_in_expr({nil, L}, _ECLn, _Bs, _Er)                             ->
  {nil, L};
replace_value_in_expr({atom, _L, _A} = VarExpr, _ECLn, _Bs, _Er)             ->
  VarExpr;
replace_value_in_expr({bin, _L, _FS} = BinExpr, _ECLn, _Bs, _Er)             ->
  %% TODO: this might need more investigation as to how to replace
  %%       the values in the sub expressions
  BinExpr;
replace_value_in_expr({cons, L, Expr0, Rest0}, ECLn, Bs, Er)                 ->
  Expr = replace_value_in_expr(Expr0, ECLn, Bs, Er),
  Rest = replace_value_in_expr(Rest0, ECLn, Bs, Er),
  {cons, L, Expr, Rest};
replace_value_in_expr({tuple, L, Exprs0}, ECLn, Bs, Er)                      ->
  Exprs = replace_value_in_exprs(Exprs0, ECLn, Bs, Er),
  %% try to convert it back to record if possible
  edts_rte_records:tuple_to_record({tuple, L, Exprs});
replace_value_in_expr({float, _, _} = VarExpr, _ECLn, _Bs, _Er)              ->
  VarExpr;
replace_value_in_expr({integer, _, _} = VarExpr, _ECLn, _Bs, _Er)            ->
  VarExpr;
replace_value_in_expr({match,L,LExpr0,RExpr0}, ECLn, Bs, Er)                 ->
  LExpr = replace_value_in_expr(LExpr0, ECLn, Bs, Er),
  RExpr = replace_value_in_expr(RExpr0, ECLn, Bs, Er),
  {match,L,LExpr,RExpr};
replace_value_in_expr({var, _, _} = VarExpr, _ECLn, Bs, _Er)                 ->
  replace_value(VarExpr, Bs);
replace_value_in_expr({op, L, Ops, Expr0}, ECLn, Bs, Er)                     ->
  Expr = replace_value_in_expr(Expr0, ECLn, Bs, Er),
  {op, L, Ops, Expr};
replace_value_in_expr({op, L, Ops, LExpr0, RExpr0}, ECLn, Bs, Er)            ->
  LExpr = replace_value_in_expr(LExpr0, ECLn, Bs, Er),
  RExpr = replace_value_in_expr(RExpr0, ECLn, Bs, Er),
  {op, L, Ops, LExpr, RExpr};
replace_value_in_expr({call, L, {atom, L, F0}, ArgL}, ECLn, Bs, Er)          ->
  F = replace_value_in_expr(F0, ECLn, Bs, Er),
  {call, L, {atom, L, F}, replace_value_in_exprs(ArgL, ECLn, Bs, Er)};
replace_value_in_expr({call, L, {var, L, _} = FunName, ArgL}, ECLn, Bs, Er)  ->
  FunVal = replace_value_in_expr(FunName, ECLn, Bs, Er),
  {call, L, FunVal, replace_value_in_exprs(ArgL, ECLn, Bs, Er)};
replace_value_in_expr({call, L, {remote, L, M0, F0}, Args0}, ECLn, Bs, Er)   ->
  M = replace_value_in_expr(M0, ECLn, Bs, Er),
  F = replace_value_in_expr(F0, ECLn, Bs, Er),
  {call, L, {remote, L, M, F}, replace_value_in_exprs(Args0, ECLn, Bs, Er)};
replace_value_in_expr({'case', L, CaseExpr0, Clauses0}, ECLn, Bs, Er)        ->
  CaseExpr = replace_value_in_expr(CaseExpr0, ECLn, Bs, Er),
  Clauses  = replace_value_in_clauses(Clauses0, ECLn, Bs, Er),
  {'case', L, CaseExpr, Clauses};
replace_value_in_expr( {string, _L, _Str} = String, _ECLn, _Bs, _Er)         ->
  String;
replace_value_in_expr( { 'try', L, Exprs0, PatternClauses0
                     , ExceptionClauses0, FinalExprs0}, ECLn, Bs, Er)        ->
  Exprs            = replace_value_in_exprs(Exprs0, ECLn, Bs, Er),
  PatternClauses   = replace_value_in_clauses( PatternClauses0
                                               , ECLn, Bs, Er),
  ExceptionClauses = replace_value_in_clauses( ExceptionClauses0
                                               , ECLn, Bs, Er),
  FinalExprs       = replace_value_in_exprs( FinalExprs0
                                             , ECLn, Bs, Er),
  {'try', L, Exprs, PatternClauses, ExceptionClauses, FinalExprs};
replace_value_in_expr({lc, L, Expr0, GenExprs0}, ECLn, Bs, Er)               ->
  Expr     = replace_value_in_expr(Expr0, ECLn, Bs, Er),
  GenExprs = replace_value_in_exprs(GenExprs0, ECLn, Bs, Er),
  {lc, L, Expr, GenExprs};
replace_value_in_expr({generate, L, ResExp, GenExp}, ECLn, Bs, Er)           ->
  { generate, L, replace_value_in_expr(ResExp, ECLn, Bs, Er)
    , replace_value_in_expr(GenExp, ECLn, Bs, Er)};
replace_value_in_expr({'receive', L, Clauses0}, ECLn, Bs, Er)                ->
  Clauses  = replace_value_in_clauses(Clauses0, ECLn, Bs, Er),
  {'receive', L, Clauses};
replace_value_in_expr({'receive', L, Clauses0, Int, Exprs0}, ECLn, Bs, Er)   ->
  Clauses  = replace_value_in_clauses(Clauses0, ECLn, Bs, Er),
  Expr     = replace_value_in_exprs(Exprs0, ECLn, Bs, Er),
  {'receive', L, Clauses, Int, Expr};
replace_value_in_expr({'fun', L, {clauses, Clauses0}}, ECLn, Bs, Er)         ->
  Clauses  = replace_value_in_lambda_clauses(Clauses0, ECLn, Bs, Er),
  {'fun', L, {clauses, Clauses}};
replace_value_in_expr({record_index, L, Name, Expr0}, ECLn, Bs, Er)          ->
  Expr = replace_value_in_expr(Expr0, ECLn, Bs, Er),
  {record_index, L, Name, Expr};
replace_value_in_expr({record_field, L, LExpr0, RExpr0}, ECLn, Bs, Er)       ->
  LExpr = replace_value_in_expr(LExpr0, ECLn, Bs, Er),
  RExpr = replace_value_in_expr(RExpr0, ECLn, Bs, Er),
  {record_field, L, LExpr, RExpr};
replace_value_in_expr({record_field, L, LExpr0, Name, RExpr0}, ECLn, Bs, Er) ->
  LExpr = replace_value_in_expr(LExpr0, ECLn, Bs, Er),
  RExpr = replace_value_in_expr(RExpr0, ECLn, Bs, Er),
  {record_field, L, LExpr, Name, RExpr};
replace_value_in_expr({record, L, Name, Fields0}, ECLn, Bs, Er)              ->
  Fields = replace_value_in_exprs(Fields0, ECLn, Bs, Er),
  case Er of
    true  ->
      edts_rte_records:record_to_tuple({record, L, Name, Fields});
    false ->
      {record, L, Name, Fields}
  end;
replace_value_in_expr({record, L, Var, Name, Fields0}, ECLn, Bs, Er)         ->
  Fields = replace_value_in_exprs(Fields0, ECLn, Bs, Er),
  case Er of
    true  ->
      edts_rte_records:record_to_tuple({record, L, Var, Name, Fields});
    false ->
      {record, L, Var, Name, Fields}
  end;
replace_value_in_expr([Statement0|T], ECLn, Bs, Er)                          ->
  Statement = replace_value_in_expr(Statement0, ECLn, Bs, Er),
  [Statement | replace_value_in_expr(T, ECLn, Bs, Er)];
replace_value_in_expr(Expr, _ECLn, _Bs, _Er)                                 ->
  error({unexpected_expression, Expr}).

replace_value({var, L, VariableName}, Bs) ->
  case lists:keyfind(VariableName, 1, Bs) of
    {VariableName, Value} -> do_replace(VariableName, Value, L);
    false                 -> {var, L, VariableName}
  end;
replace_value(Other, _Bs)                 ->
  Other.

do_replace(VarName, Value0, L) ->
  Value  = maybe_convert(Value0),
  Form   = make_replaced_form(VarName, Value),
  replace_line_num(Form, L).

%% make replaced string so that emacs can do the highlighting
%% note that we can't do this for records because otherwise
%% it will not be render properly as records but tuples.
make_replaced_form(_VarName, {record, _, _, _} = Record) ->
  Record;
make_replaced_form(VarName, Value) ->
  Str    = lists:flatten(io_lib:format( "{b,~p,s,~p,e}.", [VarName, Value])),
  Tokens = get_tokens(Str),
  {ok, [ValForm]}  = erl_parse:parse_exprs(Tokens),
  ValForm.

get_tokens(ValStr) ->
  {ok, Tokens, _} = erl_scan:string(ValStr),
  Tokens.

%%------------------------------------------------------------------------------
%% @doc for values such as Pid and Func, which are not valid
%%      erlang terms, convert them to atom and return back.
%%      For tuples, we try to convert them to records if possible
maybe_convert(Value) ->
  Str0   = lists:flatten(io_lib:format("~p", [Value])),
  Str    = string:concat(Str0, "."),
  Tokens = get_tokens(Str),
  Spec   = [ { fun() -> is_pid_tokens(Tokens) end
             , fun() -> string:concat("pid: ",Str) end}
           , { fun() -> is_func_tokens(Tokens) end
             , fun() -> string:concat("fun: ",Str) end}
           , { fun() -> is_tuple(Value) end
             , fun() ->
                   {ok, [ValForm]} = erl_parse:parse_exprs(Tokens),
                   maybe_convert_to_record(ValForm)
               end}
           ],
  convert_with_spec(Spec, Value, Str0).

maybe_convert_to_record({tuple, _L, _Fields} = Tuple) ->
  edts_rte_records:tuple_to_record(Tuple);
maybe_convert_to_record(Other) ->
  Other.

convert_with_spec([], Value, _Str)                              ->
  Value;
convert_with_spec([{Predicate, ConvertFun} | Rest], Value, Str) ->
  case Predicate() of
    true  -> ConvertFun();
    false -> convert_with_spec(Rest, Value, Str)
  end.

is_pid_tokens(Tokens) ->
  [FirstElem | _] = Tokens,
  [{dot, _}, LastElem | _] = lists:reverse(Tokens),
  is_left_arrow(FirstElem) andalso is_right_arrow(LastElem).

is_func_tokens([{'#', _}, {var, _, 'Fun'} | _Rest]) ->
  true;
is_func_tokens(_) ->
  false.

is_left_arrow({Char, _}) when Char =:= '<' ->
  true;
is_left_arrow(_) ->
  false.

is_right_arrow({Char, _}) when Char =:= '>' ->
  true;
is_right_arrow(_) ->
  false.

replace_line_num({A, _L0, C, D}, L)               ->
  {A, L, replace_line_num(C, L), replace_line_num(D, L)};
replace_line_num({A, _L0, C},    L)               ->
  {A, L, replace_line_num(C, L)};
replace_line_num({A, _L0},       L)               ->
  {A, L};
replace_line_num(Others,  L) when is_list(Others) ->
  lists:map(fun(Other) ->
                replace_line_num(Other, L)
            end, Others);
replace_line_num(Other,  _L)                      ->
  Other.

normalize_fun({'fun', L, {clauses, Clauses}}, Function) ->
  {clause,_L,ArgList,_WhenList,_Lines} = hd(Clauses),
  {function, L, Function, length(ArgList), Clauses}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
