%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_abscode_utils).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("src/horus_abscode_utils.hrl").
-include("src/horus_error.hrl").

-export([get/1,
         fold/5,
         to_erlang_code/1]).
-export([is_matching/1,
         get_expr_depth/1]).

-type vars_map() :: #{atom() => boolean()}.

%% `fold/5' state.

-record(fold, {source,
               expr_depth = 1,
               output_exprs = [],
               matching = false :: boolean(),
               % vars = #{} :: horus_abscode_utils:vars_map(),

               pre_callback :: fun(),
               post_callback :: fun(),
               priv :: any()}).

-opaque fold_state() :: #fold{}.

-record(getfun, {source = undefined,
                 fun_info,
                 decl_line,
                 first_exec_line,
                 first_exec_instr,

                 possible_target_funs = [],
                 is_target_fun = false,

                 predefined_vars = []
                }).

-record(fun_context, {expr,
                      depth,
                      referenced_vars = []}).

-type erlang_expression() :: #atom{} |
                             #attribute{} |
                             #bin{} |
                             #bin_element{} |
                             #block{} |
                             #call{} |
                             #'case'{} |
                             #char{} |
                             #clause{} |
                             #clauses{} |
                             #eof{} |
                             #float{} |
                             #'fun'{} |
                             #function{} |
                             #integer{} |
                             #map{} |
                             #map_field_assoc{} |
                             #map_field_exact{} |
                             #match{} |
                             #nil{} |
                             {op, any(), any(), any()} |
                             {op, any(), any(), any(), any()} |
                             #'receive'{} |
                             #record{} |
                             #record_field{} |
                             #remote{} |
                             #string{} |
                             #'try'{} |
                             #tuple{} |
                             #var{}.

%% Internal expressions.
%% They must not end up in the final patched abstract code.

-record(inner_exprs_start, {index :: pos_integer(),
                            matching :: boolean() | inherit}).

-record(inner_exprs_end, {index,
                          is_list,
                          old_matching,
                          output_exprs = [] :: [expression()],
                          rest = [] :: [any()]}).

-record(scope_exit, {vars :: horus_abscode_utils:vars_map()}).

-type internal_expression() :: #inner_exprs_start{} |
                               #inner_exprs_end{} |
                               #scope_exit{}.

-type expression() :: erlang_expression() | internal_expression().

-export_type([vars_map/0,
              fold_state/0,
              erlang_expression/0,
              internal_expression/0,
              expression/0]).

get(Fun) when is_function(Fun) ->
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module} = FunInfo,
    {StartLine,
     FirstExecLine,
     FirstInstruction} = horus_asm_utils:get_fun_start_lines(Fun),
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code(Beam),
    %% Get:
    %% 1. keep all match expressions to locate variables defined outside of a function:
    %%      * left hand side: does it have variables?
    %%      * righ hand side: does it only have literals and variables?
    %% 2. locate all referenced variables in the function:
    %% 3. list all referenced but undefined variables
    %% 4. for each retained match expression that defines undefined variables:
    %%      a. rename other unused variables to `_'
    %% 5. add patched match expressions at the beginning of the clause body
    %% 6. add undefined variables left to args, sorted alphabetically
    %%
    %% Caveat: can a literal-based variable still be passed in the environment?
    GF = #getfun{fun_info = FunInfo,
                 decl_line = StartLine,
                 first_exec_line = FirstExecLine,
                 first_exec_instr = FirstInstruction},
    case fold(AbstractCode, Fun, fun pre_get/3, fun post_get/3, GF) of
        {ok, _, FunAbstractCode} ->
            {ok, FunAbstractCode}
    end;
get({Module, Name, Arity} = MFA) ->
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code(Beam),
    PreCallback = fun
                      (#attribute{name = file} = Expr,
                       _Fold, Priv) ->
                          Priv1 = Priv#{source_file => Expr},
                          {continue, Expr, Priv1};
                      (#function{name = Name1, arity = Arity1} = Expr,
                       _Fold, Priv)
                        when not is_map_key(abstract_code, Priv) andalso
                             Name1 =:= Name andalso Arity1 =:= Arity ->
                          Priv1 = Priv#{abstract_code => Expr},
                          {abort, Priv1};
                      (Expr, _Fold, Priv) ->
                          {skip, Expr, Priv}
                  end,
    case fold(AbstractCode, MFA, PreCallback, none, #{}) of
        {ok, _, #{source_file := SourceFile, abstract_code := AbstractCode1}} ->
            AbstractCode2 = [SourceFile, AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, #{abstract_code := AbstractCode1}} ->
            AbstractCode2 = [AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, _Priv} ->
            Reason = ?horus_error(
                        fun_not_found,
                        #{function => MFA,
                          module_abstract_code => AbstractCode}),
            {error, Reason}
    end.

pre_get(
  #attribute{name = file} = Expr,
  _Fold,
  Priv) ->
    Priv1 = Priv#getfun{source = Expr},
    {continue, Priv1};
pre_get(
  #'fun'{location = {StartLine, _StartCol}} = Expr,
  Fold,
  #getfun{decl_line = StartLine,
          possible_target_funs = PossibleTargetFuns} = Priv) ->
    FunDepth = get_expr_depth(Fold),
    FunContext = #fun_context{expr = Expr,
                              depth = FunDepth},
    PossibleTargetFuns1 = [FunContext | PossibleTargetFuns],
    Priv1 = Priv#getfun{possible_target_funs = PossibleTargetFuns1},
    {continue, Priv1};
pre_get(
  #clause{},
  Fold,
  #getfun{possible_target_funs = [FunContext | OuterFunContexts]} = Priv) ->
    #fun_context{depth = FunDepth,
                 referenced_vars = RefdVarsPerClause} = FunContext,
    ClauseDepth = get_expr_depth(Fold),
    case ClauseDepth =:= FunDepth + 2 of
        true ->
            RefdVarsPerClause1 = [#{} | RefdVarsPerClause],
            FunContext1 = FunContext#fun_context{referenced_vars = RefdVarsPerClause1},
            Priv1 = Priv#getfun{possible_target_funs = [FunContext1 | OuterFunContexts]},
            {continue, Priv1};
        false ->
            {continue, Priv}
    end;
pre_get(
  enter_scope,
  _Fold,
  #getfun{predefined_vars = OuterScopes} = Priv) ->
    NewScope = case OuterScopes of
                   []                 -> [];
                   [CurrentScope | _] -> CurrentScope
               end,
    OuterScopes1 = [NewScope | OuterScopes],
    Priv1 = Priv#getfun{predefined_vars = OuterScopes1},
    {continue, Priv1};
pre_get(
  #var{name = Name},
  Fold,
  #getfun{possible_target_funs = [FunContext | OuterFunContexts]} = Priv) ->
    Matching = is_matching(Fold),
    % logger:alert("Var ~s, matching = ~s", [Name, Matching]),
    #fun_context{referenced_vars = [RefdVars | RefdVarsPerClause]} = FunContext,
    RefdVars1 = case RefdVars of
                    #{Name := _} -> RefdVars;
                    _            -> RefdVars#{Name => Matching}
                end,
    RefdVarsPerClause1 = [RefdVars1 | RefdVarsPerClause],
    FunContext1 = FunContext#fun_context{referenced_vars = RefdVarsPerClause1},
    Priv1 = Priv#getfun{possible_target_funs = [FunContext1 | OuterFunContexts]},
    {continue, Priv1};
pre_get(Expr, _Fold, Priv) ->
    Priv1 = handle_first_executable_expr(Expr, Priv),
    {continue, Priv1}.

handle_first_executable_expr(
  Expr,
  #getfun{first_exec_line = FirstExecLine,
          possible_target_funs = [_ | _],
          is_target_fun = false} = Priv)
  when is_record(Expr, atom) orelse
       is_record(Expr, bin) orelse
       is_record(Expr, call) orelse
       is_record(Expr, char) orelse
       is_record(Expr, cons) orelse
       is_record(Expr, float) orelse
       is_record(Expr, 'fun') orelse
       is_record(Expr, integer) orelse
       is_record(Expr, map) orelse
       is_record(Expr, match) orelse
       is_record(Expr, nil) orelse
       element(1, Expr) =:= op orelse
       is_record(Expr, record) orelse
       is_record(Expr, string) orelse
       is_record(Expr, tuple) orelse
       is_record(Expr, var) ->
    ExecLine = element(2, Expr),
    % logger:alert("Expr = ~s @ ~0p~nFirstExecLine = ~b", [element(1, Expr), ExecLine, FirstExecLine]),
    case ExecLine of
        {FirstExecLine, _} ->
            Priv1 = Priv#getfun{is_target_fun = true},
            Priv1;
        _ ->
            Priv
    end;
handle_first_executable_expr(_Expr, Priv) ->
    % logger:alert("Expr not executable = ~p", [_Expr]),
    Priv.

post_get(
  #'fun'{} = Expr,
  _Fold,
  #getfun{source = Source,
          fun_info = FunInfo,
          possible_target_funs = [FunContext | OuterFunContexts],
          is_target_fun = IsTargetFun,
          predefined_vars = [CurrentScope | _]} = Priv) ->
    case IsTargetFun of
        true ->
            #fun_context{referenced_vars = RefdVarsPerClause} = FunContext,
            RefdVarsPerClause1 = lists:reverse(RefdVarsPerClause),
            %% Leave `CurrentScope' in reversed order for upcoming handling of
            %% unbound variables.
            Expr1 = patch_anonymous_function(
                      Expr, FunInfo, RefdVarsPerClause1, CurrentScope),
            {abort, [Source, Expr1]};
        false ->
            Priv1 = Priv#getfun{possible_target_funs = OuterFunContexts},
            {continue, Priv1}
    end;
post_get(
  #match{} = Expr,
  _Fold,
  #getfun{predefined_vars = [CurrentScope | OuterScopes]} = Priv) ->
    CurrentScope1 = [Expr | CurrentScope],
    Priv1 = Priv#getfun{predefined_vars = [CurrentScope1 | OuterScopes]},
    {continue, Priv1};
post_get(
  exit_scope,
  _Fold,
  #getfun{predefined_vars = [_CurrentScope | OuterScopes]} = Priv) ->
    Priv1 = Priv#getfun{predefined_vars = OuterScopes},
    {continue, Priv1};
post_get(_Expr, _Fold, Priv) ->
    {continue, Priv}.

patch_anonymous_function(
  #'fun'{code = #clauses{clauses = Clauses}} = Expr,
  FunInfo, RefdVarsPerClause, PredefinedVars) ->
    patch_anonymous_function_clauses(
      Expr, FunInfo, Clauses, RefdVarsPerClause, PredefinedVars, []).

patch_anonymous_function_clauses(
  Expr, #{env := Env} = FunInfo,
  [#clause{args = Args, body = Body} = Clause | ClausesRest],
  [RefdVars | RefdVarsRest],
  PredefinedVars, PatchedClauses) ->
    UnboundVars1 = maps:fold(
                     fun
                         (_Name, true, Acc) -> Acc;
                         (Name, false, Acc) -> [Name | Acc]
                     end, [], RefdVars),
    logger:alert("Unbound variables 1: ~1p", [UnboundVars1]),
    {VarsInitExprs,
     UnboundVars2} = take_unbound_vars_from_predefined_vars(
                       RefdVars, UnboundVars1, PredefinedVars),
    logger:alert("Unbound variables 2: ~1p", [UnboundVars2]),

    UnboundVars3 = lists:reverse(lists:sort(UnboundVars2)),
    ?assertEqual(length(Env), length(UnboundVars3)),
    ArgsFromEnv = [#var{location = 0, name = Name} || Name <- UnboundVars3],
    Args1 = Args ++ ArgsFromEnv,
    Body1 = VarsInitExprs ++ Body,
    Clause1 = Clause#clause{args = Args1, body = Body1},
    PatchedClauses1 = [Clause1 | PatchedClauses],
    patch_anonymous_function_clauses(
      Expr, FunInfo, RefdVarsRest, ClausesRest, PredefinedVars,
      PatchedClauses1);
patch_anonymous_function_clauses(
  Expr, _FunInfo, [], [], _PredefinedVars, PatchedClauses) ->
    PatchedClauses1 = lists:reverse(PatchedClauses),
    Expr1 = Expr#'fun'{code = #clauses{clauses = PatchedClauses1}},
    Expr1.

take_unbound_vars_from_predefined_vars(RefdVars, UnboundVars, PredefinedVars) ->
    take_unbound_vars_from_predefined_vars(
      RefdVars, UnboundVars, PredefinedVars, []).

take_unbound_vars_from_predefined_vars(
  _RefdVars, UnboundVars, PredefinedVars, VarsInitExprs)
  when UnboundVars =:= [] orelse PredefinedVars =:= [] ->
    {VarsInitExprs, UnboundVars};
take_unbound_vars_from_predefined_vars(
  RefdVars, UnboundVars,
  [#match{left = Left, right = Right} = Expr | Rest],
  VarsInitExprs) ->
    %% TODO: Var1 = Var2 = Expr
    Source = undefined, % XXX
    LeftPreCallback = none,
    LeftPostCallback = fun
                           (#var{name = Name} = Var, _Fold, {Allowed, UV1}) ->
                               case lists:member(Name, UV1) of
                                   true ->
                                       UV2 = UV1 -- [Name],
                                       {continue, {Allowed, UV2}};
                                   false ->
                                       case maps:is_key(Name, RefdVars) of
                                           true ->
                                               {continue, {Allowed, UV1}};
                                           false ->
                                               Var1 = Var#var{name = '_'},
                                               {continue, Var1, {Allowed, UV1}}
                                       end
                               end;
                           (_Expr, _Fold, Acc) ->
                               {continue, Acc}
                       end,
    {ok, Left1, {LeftAllowed, UnboundVars1}} = fold(
                                                 Left, Source,
                                                 LeftPreCallback, LeftPostCallback,
                                                 {true, UnboundVars}),
    RightPreCallback = fun
                           (#var{name = Name}, _Fold, {Allowed, UV1}) ->
                               case lists:member(Name, UV1) of
                                   true ->
                                       {continue, {Allowed, UV1}};
                                   false ->
                                       UV2 = [Name | UV1],
                                       {continue, {Allowed, UV2}}
                               end;
                           (E, _Fold, Acc)
                             when is_record(E, atom) orelse
                                  is_record(E, char) orelse
                                  is_record(E, eof) orelse
                                  is_record(E, float) orelse
                                  is_record(E, integer) orelse
                                  is_record(E, nil) orelse
                                  is_record(E, string) orelse
                                  is_record(E, var) ->
                               {continue, Acc};
                           (_Expr, _Fold, {_Allowed, UV1}) ->
                               {continue, {false, UV1}}
                       end,
    RightPostCallback = none,
    {ok, Right1, {RightAllowed, UnboundVars2}} = fold(
                                                   Right, Source,
                                                   RightPreCallback, RightPostCallback,
                                                   {true, UnboundVars1}),
    case LeftAllowed andalso RightAllowed of
        true ->
            case UnboundVars2 of
                UnboundVars ->
                    take_unbound_vars_from_predefined_vars(
                      RefdVars, UnboundVars, Rest, VarsInitExprs);
                _ ->
                    Expr1 = Expr#match{left = Left1, right = Right1},
                    VarsInitExprs1 = [Expr1 | VarsInitExprs],
                    take_unbound_vars_from_predefined_vars(
                      RefdVars, UnboundVars2, Rest, VarsInitExprs1)
            end;
        false ->
            take_unbound_vars_from_predefined_vars(
              RefdVars, UnboundVars, Rest, VarsInitExprs)
    end.

% get(Fun) when is_function(Fun) ->
%     FunInfo = horus_erlfun_utils:info(Fun),
%     #{module := Module} = FunInfo,
%     %% FIXME: `StartLine' not enough. Use the first asm instruction and its
%     %% line number to determine which function on that same line is the best
%     %% match.
%     {StartLine, FirstExprLine, _FirstInstruction} = horus_asm_utils:get_fun_start_lines(Fun),
%     Beam = horus_beam_utils:get_beam(Module),
%     AbstractCode = horus_beam_utils:get_abstract_code(Beam),
%     % logger:alert("Module = ~p~nFunInfo = ~p~nStartLine = ~b~nFirstInstruction = ~0p @ ~b", [AbstractCode, FunInfo, StartLine, _FirstInstruction, FirstExprLine]),
%     %% Get:
%     %% 1. keep all match expressions to locate variables defined outside of a function:
%     %%    * left hand side: does it have variables?
%     %%    * righ hand side: does it only have literals and variables?
%     %% 2. locate all referenced variables in the function:
%     %%    * 
%     %% 3. k
%     PreCallback = fun
%                       (#attribute{name = file} = Expr,
%                        _Fold, Priv) ->
%                           Priv1 = Priv#{source_file => Expr},
%                           {continue, Expr, Priv1};
%                       (#'fun'{location = {StartLine1, _StartCol},
%                               code = #clauses{clauses = Clauses}} = Expr,
%                        _Fold, #{scopes := [Scope | _]} = Priv)
%                         when not is_map_key(abstract_code, Priv) andalso
%                              StartLine1 =:= StartLine ->
%                           FirstExpr = find_first_executable_expr(Clauses),
%                           % logger:alert("Firt executable expr = ~p", [FirstExpr]),
%                           case element(2, FirstExpr) of
%                               {FirstExprLine, _} ->
%                                   Scope1 = lists:reverse(Scope),
%                                   Priv1 = Priv#{abstract_code => Expr,
%                                                 scope => Scope1},
%                                   {abort, Priv1};
%                               _ ->
%                                   {continue, Expr, Priv}
%                           end;
%                       (#var{name = Name} = Expr,
%                        Fold,
%                        #getfun{referenced_vars = RefdVars} = Priv) ->
%                           Matching = is_matching(Fold),
%                           RefdVars1 = case RefdVars of
%                                       #{Name := _} -> RefdVars;
%                                       _            -> RefdVars#{Name => Matching}
%                                   end,
%                           Priv1 = Priv#getfun{referenced_vars = RefdVars1},
%                           {continue, Expr, Priv1};
%                       (enter_scope = Expr, _Fold, #{scopes := Scopes} = Priv) ->
%                           Scopes1 = case Scopes of
%                                         []         -> [[]];
%                                         [Vars | _] -> [Vars | Scopes]
%                                     end,
%                           Priv1 = Priv#{scopes => Scopes1},
%                           {continue, Expr, Priv1};
%                       (Expr, _Fold, Priv) ->
%                           {continue, Expr, Priv}
%                   end,
%     PostCallback = fun
%                       (#match{} = Expr, _Fold, #{scopes := [CurrentScope | Scopes]} = Priv) ->
%                           CurrentScope1 = [Expr | CurrentScope],
%                           Priv1 = Priv#{scopes => [CurrentScope1 | Scopes]},
%                           {continue, Expr, Priv1};
%                       (exit_scope = Expr, _Fold, #{scopes := Scopes} = Priv) ->
%                            Scopes1 = tl(Scopes),
%                            Priv1 = Priv#{scopes => Scopes1},
%                           {continue, Expr, Priv1};
%                        (Expr, _Fold, Priv) ->
%                            {continue, Expr, Priv}
%                    end,
%     case fold(AbstractCode, Fun, PreCallback, PostCallback, #{scopes => []}) of
%         {ok, _, #{abstract_code := AbstractCode1}} ->
%             AbstractCode2 = [AbstractCode1],
%             {ok, AbstractCode2};
%         {ok, _, _Priv} ->
%             Reason = ?horus_error(
%                         fun_not_found,
%                         #{function => Fun,
%                           fun_info => FunInfo,
%                           start_line => StartLine,
%                           module_abstract_code => AbstractCode}),
%             {error, Reason}
%     end;
% get({Module, Name, Arity} = MFA) ->
%     Beam = horus_beam_utils:get_beam(Module),
%     AbstractCode = horus_beam_utils:get_abstract_code(Beam),
%     PreCallback = fun
%                       (#attribute{name = file} = Expr,
%                        _Fold, Priv) ->
%                           Priv1 = Priv#{source_file => Expr},
%                           {continue, Expr, Priv1};
%                       (#function{name = Name1, arity = Arity1} = Expr,
%                        _Fold, Priv)
%                         when not is_map_key(abstract_code, Priv) andalso
%                              Name1 =:= Name andalso Arity1 =:= Arity ->
%                           Priv1 = Priv#{abstract_code => Expr},
%                           {abort, Priv1};
%                       (Expr, _Fold, Priv) ->
%                           {skip, Expr, Priv}
%                   end,
%     case fold(AbstractCode, MFA, PreCallback, none, #{}) of
%         {ok, _, #{source_file := SourceFile, abstract_code := AbstractCode1}} ->
%             AbstractCode2 = [SourceFile, AbstractCode1],
%             {ok, AbstractCode2};
%         {ok, _, #{abstract_code := AbstractCode1}} ->
%             AbstractCode2 = [AbstractCode1],
%             {ok, AbstractCode2};
%         {ok, _, _Priv} ->
%             Reason = ?horus_error(
%                         fun_not_found,
%                         #{function => MFA,
%                           module_abstract_code => AbstractCode}),
%             {error, Reason}
%     end.
%
% find_first_executable_expr([Expr | _])
%   when is_record(Expr, atom) orelse
%        is_record(Expr, bin) orelse
%        is_record(Expr, call) orelse
%        is_record(Expr, char) orelse
%        is_record(Expr, cons) orelse
%        is_record(Expr, float) orelse
%        is_record(Expr, 'fun') orelse
%        is_record(Expr, integer) orelse
%        is_record(Expr, map) orelse
%        is_record(Expr, match) orelse
%        is_record(Expr, nil) orelse
%        element(1, Expr) =:= op orelse
%        is_record(Expr, record) orelse
%        is_record(Expr, string) orelse
%        is_record(Expr, tuple) orelse
%        is_record(Expr, var) ->
%     Expr;
% find_first_executable_expr([#block{expressions = Exprs} | _]) ->
%     find_first_executable_expr(Exprs);
% find_first_executable_expr([#clause{body = Body} | _]) ->
%     find_first_executable_expr(Body);
% find_first_executable_expr([#clauses{clauses = Clauses} | _]) ->
%     find_first_executable_expr(Clauses);
% find_first_executable_expr([#'if'{clauses = Clauses} | _]) ->
%     find_first_executable_expr(Clauses);
% find_first_executable_expr([#'receive'{clauses = Clauses} | _]) ->
%     find_first_executable_expr(Clauses);
% find_first_executable_expr([#'try'{block = Block} | _]) ->
%     find_first_executable_expr(Block).

fold(AbstractCode, Source, PreCallback, PostCallback, Priv)
  when is_list(AbstractCode) ->
    Fold = #fold{source = Source,
                 pre_callback = PreCallback,
                 post_callback = PostCallback,
                 priv = Priv},
    case fold(AbstractCode, Fold) of
        {ok, #fold{output_exprs = AbstractCode1, priv = Priv1}} ->
            AbstractCode2 = lists:reverse(AbstractCode1),
            {ok, AbstractCode2, Priv1};
        {abort, #fold{priv = Priv1}} ->
            {ok, none, Priv1}
    end;
fold(AbstractCode, Source, PreCallback, PostCallback, Priv) ->
    case fold([AbstractCode], Source, PreCallback, PostCallback, Priv) of
        {ok, [AbstractCode1], Priv1} ->
            {ok, AbstractCode1, Priv1};
        {ok, none, _Priv1} = Ret ->
            Ret
    end.

fold([Expr | Rest], Fold)
  when is_record(Expr, atom) orelse
       is_record(Expr, char) orelse
       is_record(Expr, eof) orelse
       is_record(Expr, float) orelse
       is_record(Expr, integer) orelse
       is_record(Expr, nil) orelse
       is_record(Expr, string) orelse
       is_record(Expr, var) ->
    handle_expr(Expr, [], true, Rest, Fold);
fold([Expr | Rest], Fold)
  when is_record(Expr, attribute) ->
    handle_expr(Expr, [], true, Rest, Fold);
fold([#bin{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #bin.elements,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#bin_element{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #bin_element.value,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#block{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #block.expressions,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#call{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #call.call,
                                     matching = false},
                  #inner_exprs_start{index = #call.args,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#'case'{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #'case'.input,
                                     matching = false},
                  #inner_exprs_start{index = #'case'.clauses,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#clause{} = Expr | Rest], #fold{matching = false} = Fold) ->
    InnerExprs = [#inner_exprs_start{index = #clause.args,
                                     matching = true},
                  #inner_exprs_start{index = #clause.body,
                                     matching = false}],
    enter_scope(Expr, InnerExprs, true, Rest, Fold);
fold([#clauses{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #clauses.clauses,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#cons{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #cons.head,
                                     matching = inherit},
                  #inner_exprs_start{index = #cons.tail,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#'fun'{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #'fun'.code,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#function{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #function.clauses,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#'if'{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #'if'.clauses,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#map{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #map.elements,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#map_field_assoc{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #map_field_assoc.key,
                                     matching = false},
                  #inner_exprs_start{index = #map_field_assoc.value,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#map_field_exact{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #map_field_exact.key,
                                     matching = false},
                  #inner_exprs_start{index = #map_field_exact.value,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#match{} = Expr | Rest], #fold{} = Fold) ->
    InnerExprs = [#inner_exprs_start{index = #match.left,
                                     matching = true},
                  #inner_exprs_start{index = #match.right,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([{op, _Location, _Operator, _Operand} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = 4,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([{op, _Location, _Operator, _LeftOperand, _RightOperand} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = 4,
                                     matching = false},
                  #inner_exprs_start{index = 5,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#'receive'{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #'receive'.clauses,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#record{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #record.fields,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#record_field{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #record_field.name,
                                     matching = false},
                  #inner_exprs_start{index = #record_field.value,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#remote{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #remote.module,
                                     matching = false},
                  #inner_exprs_start{index = #remote.function,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#'try'{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #'try'.block,
                                     matching = false},
                  #inner_exprs_start{index = #'try'.unnamed1,
                                     matching = false},
                  #inner_exprs_start{index = #'try'.'catch',
                                     matching = false},
                  #inner_exprs_start{index = #'try'.unnamed2,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#tuple{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #tuple.elements,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
% fold(
%   [#var{name = Name} = Expr | Rest],
%   #fold{matching = Matching, vars = Vars} = Fold) ->
%     Vars1 = case Vars of
%                 #{Name := _} -> Vars;
%                 _            -> Vars#{Name => Matching}
%             end,
%     Fold1 = Fold#fold{vars = Vars1},
%     handle_expr(Expr, [], false, Rest, Fold1);

fold(
  [#inner_exprs_start{index = Index, matching = Matching} | Rest],
  #fold{matching = OldMatching,
        expr_depth = ExprDepth,
        output_exprs = [ParentExpr | _] = OutputExprs} = Fold) ->
    InnerExprs = element(Index, ParentExpr),
    IsList = is_list(InnerExprs),
    InnerExprsEnd = #inner_exprs_end{index = Index,
                                     is_list = IsList,
                                     old_matching = OldMatching,
                                     output_exprs = OutputExprs},
    Fold1 = Fold#fold{expr_depth = ExprDepth + 1,
                      output_exprs = []},
    Fold2 = set_matching(Matching, Fold1),
    case IsList of
        true  -> fold(InnerExprs ++ [InnerExprsEnd | Rest], Fold2);
        false -> fold([InnerExprs, InnerExprsEnd | Rest], Fold2)
    end;
fold(
  [#inner_exprs_end{index = Index,
                    is_list = IsList,
                    old_matching = OldMatching,
                    output_exprs = [ParentExpr | OutputExprs]} | Rest],
  #fold{expr_depth = ExprDepth, output_exprs = InnerExprs} = Fold) ->
    InnerExprs1 = case IsList of
                      true ->
                          lists:reverse(InnerExprs);
                      false ->
                          [InnerExpr] = InnerExprs,
                          InnerExpr
                  end,
    ParentExpr1 = setelement(Index, ParentExpr, InnerExprs1),
    OutputExprs1 = [ParentExpr1 | OutputExprs],
    Fold1 = Fold#fold{expr_depth = ExprDepth - 1,
                      output_exprs = OutputExprs1},
    Fold2 = set_matching(OldMatching, Fold1),
    fold(Rest, Fold2);
fold(
  [#scope_exit{} = Expr | Rest],
  Fold) ->
    exit_scope(Expr, Rest, Fold);
fold(
  [post_callback | Rest],
  #fold{output_exprs = [Expr | OutputExprs]} = Fold) ->
    case post_callback(Expr, Fold) of
        {Step, Fold1} when Step =:= continue orelse Step =:= skip ->
            OutputExprs1 = [Expr | OutputExprs],
            Fold2 = Fold1#fold{output_exprs = OutputExprs1},
            fold(Rest, Fold2);
        {Step, Expr1, Fold1} when Step =:= continue orelse Step =:= skip ->
            OutputExprs1 = [Expr1 | OutputExprs],
            Fold2 = Fold1#fold{output_exprs = OutputExprs1},
            fold(Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;

fold(
  [UnknownExpression | _Rest],
  #fold{source = Source} = _Fold) ->
    Source1 = case is_function(Source) of
                  true  -> horus_erlfun_utils:info(Source);
                  false -> Source
              end,
    erlang:error(
      ?horus_exception(unknow_expression, #{expression => UnknownExpression,
                                            source => Source1}));
fold(
  [],
  Fold) ->
    {ok, Fold}.

handle_expr(Expr, InnerExprIdxs, true = _UseCallbacks, Rest, Fold) ->
    Rest1 = [post_callback | Rest],
    case pre_callback(Expr, Fold) of
        {continue, Fold1} ->
            do_handle_expr(Expr, InnerExprIdxs, Rest1, Fold1);
        {continue, Expr1, Fold1} ->
            do_handle_expr(Expr1, InnerExprIdxs, Rest1, Fold1);
        {skip, Fold1} ->
            do_handle_expr(Expr, [], Rest1, Fold1);
        {skip, Expr1, Fold1} ->
            do_handle_expr(Expr1, [], Rest1, Fold1);
        {abort, _Fold1} = Ret ->
            Ret
    end;
handle_expr(Expr, InnerExprIdxs, false = _UseCallbacks, Rest, Fold) ->
    do_handle_expr(Expr, InnerExprIdxs, Rest, Fold).

do_handle_expr(Expr, [], Rest, Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
do_handle_expr(Expr, InnerExprIdxs, Rest, Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(InnerExprIdxs ++ Rest, Fold1).

set_matching(inherit, Fold) ->
    Fold;
set_matching(Matching, Fold) when is_boolean(Matching) ->
    Fold1 = Fold#fold{matching = Matching},
    Fold1.

% enter_scope(Expr, InnerExprs, UseCallbacks, Rest, #fold{vars = Vars} = Fold) ->
enter_scope(Expr, InnerExprs, UseCallbacks, Rest, #fold{} = Fold) ->
    % logger:alert("ENTER SCOPE, Expr = ~p", [Expr]),
    % ExitScope = #scope_exit{vars = Vars},
    ExitScope = #scope_exit{},
    Rest1 = [ExitScope | Rest],
    case pre_callback(enter_scope, Fold) of
        {Step, Fold1}
          when Step =:= continue orelse Step =:= skip ->
            handle_expr(Expr, InnerExprs, UseCallbacks, Rest1, Fold1);
        {Step, enter_scope, Fold1}
          when Step =:= continue orelse Step =:= skip ->
            handle_expr(Expr, InnerExprs, UseCallbacks, Rest1, Fold1);
        {abort, _Fold1} = Ret ->
            Ret
    end.

% exit_scope(#scope_exit{vars = Vars}, Rest, Fold) ->
%     % logger:alert("EXIT SCOPE, Rest = ~p", [hd(Rest)]),
%     Fold1 = Fold#fold{vars = Vars},
%     case post_callback(exit_scope, Fold1) of
%         {Step, exit_scope, Fold2}
%           when Step =:= continue orelse Step =:= skip ->
%             fold(Rest, Fold2);
%         {abort, _Fold2} = Ret ->
%             Ret
%     end.
exit_scope(#scope_exit{}, Rest, Fold) ->
    % logger:alert("EXIT SCOPE, Rest = ~p", [hd(Rest)]),
    case post_callback(exit_scope, Fold) of
        {Step, Fold1}
          when Step =:= continue orelse Step =:= skip ->
            fold(Rest, Fold1);
        {Step, exit_scope, Fold1}
          when Step =:= continue orelse Step =:= skip ->
            fold(Rest, Fold1);
        {abort, _Fold1} = Ret ->
            Ret
    end.

push_expression(Expression, #fold{output_exprs = OutputExprs} = Fold) ->
    OutputExprs1 = [Expression | OutputExprs],
    Fold1 = Fold#fold{output_exprs = OutputExprs1},
    Fold1.

pre_callback(Expr, #fold{pre_callback = PreCallback} = Fold) ->
    run_callback(PreCallback, Expr, Fold).

post_callback(Expr, #fold{post_callback = PostCallback} = Fold) ->
    run_callback(PostCallback, Expr, Fold).

run_callback(none, Expr, Fold) ->
    {continue, Expr, Fold};
run_callback(Callback, Expr, #fold{source = Source, priv = Priv} = Fold) ->
    try Callback(Expr, Fold, Priv) of
        {Step, Priv1}
          when Step =:= continue orelse Step =:= skip orelse Step =:= abort ->
            Fold1 = Fold#fold{priv = Priv1},
            {Step, Fold1};
        {Step, Expr1, Priv1}
          when Step =:= continue orelse Step =:= skip ->
            Fold1 = Fold#fold{priv = Priv1},
            {Step, Expr1, Fold1}
    catch
        Class:Reason:Stacktrace ->
            Source1 = case is_function(Source) of
                          true  -> horus_erlfun_utils:info(Source);
                          false -> Source
                      end,
            logger:alert("Source = ~0p", [Source1]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

is_matching(#fold{matching = Matching}) ->
    Matching.

get_expr_depth(#fold{expr_depth = ExprDepth}) ->
    ExprDepth.

% get_vars(#fold{vars = Vars}) ->
%     Vars.

to_erlang_code(AbstractCode) ->
    Form = erl_syntax:form_list(AbstractCode),
    SourceCode = erl_prettypr:format(Form),
    SourceCode.
