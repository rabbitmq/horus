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

-export([get/1,
         get/3,
         fold/4,
         to_erlang_code/1]).

-type vars_map() :: #{atom() => boolean()}.

%% `fold/2' state.

-record(fold, {matching = false :: boolean(),
               vars = #{} :: horus_abscode_utils:vars_map(),
               output_exprs = [],

               pre_callback :: fun(),
               post_callback :: fun(),
               priv :: any()}).

-type erlang_expression() :: #attribute{} |
                             #bin{} |
                             #bin_element{} |
                             #call{} |
                             #clause{} |
                             #'fun'{} |
                             #function{} |
                             #integer{} |
                             #match{} |
                             #op{} |
                             #tuple{} |
                             #var{}.

%% Internal expressions.
%% They must not end up in the final patched abstract code.

-record(inner_exprs_end, {callback = undefined :: fun(),
                          output_exprs = [] :: [expression()],
                          rest = [] :: [any()]}).

-record(scope_exit, {vars :: horus_abscode_utils:vars_map()}).

-type internal_expression() :: #inner_exprs_end{} |
                               #scope_exit{}.

-type expression() :: erlang_expression() | internal_expression().

-export_type([vars_map/0,
              erlang_expression/0,
              internal_expression/0,
              expression/0]).

get(Fun) ->
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module} = FunInfo,
    StartLine = horus_asm_utils:get_fun_start_line(Fun),
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code(Beam),
    PreCallback = fun
                      (#attribute{name = file} = Expr,
                       _Vars, Priv) ->
                          Priv1 = Priv#{source_file => Expr},
                          {continue, Expr, Priv1};
                      (#'fun'{location = {StartLine1, _StartCol}} = Expr,
                       _Vars, Priv)
                        when not is_map_key(abstract_code, Priv) andalso
                             StartLine1 =:= StartLine ->
                          Priv1 = Priv#{abstract_code => Expr},
                          {abort, Priv1};
                      (Expr, _Vars, Priv) ->
                          {continue, Expr, Priv}
                  end,
    logger:alert("Module abstract code: ~p", [AbstractCode]),
    case fold(AbstractCode, PreCallback, none, #{}) of
        {ok, _, #{source_file := SourceFile, abstract_code := AbstractCode1}} ->
            AbstractCode2 = [SourceFile, AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, #{abstract_code := AbstractCode1}} ->
            AbstractCode2 = [AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, _Priv} ->
            {error, not_found}
    end.

get(Module, Name, Arity) ->
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code(Beam),
    PreCallback = fun
                      (#attribute{name = file} = Expr,
                       _Vars, Priv) ->
                          Priv1 = Priv#{source_file => Expr},
                          {continue, Expr, Priv1};
                      (#function{name = Name1, arity = Arity1} = Expr,
                       _Vars, Priv)
                        when not is_map_key(abstract_code, Priv) andalso
                             Name1 =:= Name andalso Arity1 =:= Arity ->
                          Priv1 = Priv#{abstract_code => Expr},
                          {abort, Priv1};
                      (_Expr, _Vars, Priv) ->
                          {skip, Priv}
                  end,
    case fold(AbstractCode, PreCallback, none, #{}) of
        {ok, _, #{source_file := SourceFile, abstract_code := AbstractCode1}} ->
            AbstractCode2 = [SourceFile, AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, #{abstract_code := AbstractCode1}} ->
            AbstractCode2 = [AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, _Priv} ->
            {error, not_found}
    end.

fold(AbstractCode, PreCallback, PostCallback, Priv) when is_list(AbstractCode) ->
    Fold = #fold{pre_callback = PreCallback,
                 post_callback = PostCallback,
                 priv = Priv},
    case fold(AbstractCode, Fold) of
        {ok, #fold{output_exprs = AbstractCode1, priv = Priv1}} ->
            AbstractCode2 = lists:reverse(AbstractCode1),
            {ok, AbstractCode2, Priv1};
        {abort, #fold{priv = Priv1}} ->
            {ok, none, Priv1}
    end;
fold(AbstractCode, PreCallback, PostCallback, Priv) ->
    case fold([AbstractCode], PreCallback, PostCallback, Priv) of
        {ok, [AbstractCode1], Priv1} ->
            {ok, AbstractCode1, Priv1};
        {ok, none, _Priv1} = Ret ->
            Ret
    end.

fold(
  [#atom{} = Expr | Rest],
  Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
fold(
  [#attribute{} = Expr | Rest],
  Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            case post_callback(Expr1, Fold1) of
                {continue, Expr2, Fold2} ->
                    Fold3 = push_expression(Expr2, Fold2),
                    fold(Rest, Fold3);
                {abort, _Fold2} = Ret ->
                    Ret
            end;
        {skip, Fold1} ->
            Fold2 = push_expression(Expr, Fold1),
            fold(Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;
fold(
  [#bin{} = Expr | Rest],
  Fold) ->
    handle_expr_with_one_inner_expr_list(Expr, #bin.elements, Rest, Fold);
fold(
  [#bin_element{} = Expr | Rest],
  Fold) ->
    handle_expr_with_one_inner_expr(Expr, #bin_element.value, Rest, Fold);
fold(
  [#call{args = Args} = Expr | Rest],
  Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            ArgsEndCallback = fun(Args1, ArgsRest, ArgsFold) ->
                                      {CallExpr, ArgsFold1} = pop_expression(ArgsFold),
                                      CallExpr1 = CallExpr#call{args = Args1},
                                      case post_callback(CallExpr1, ArgsFold1) of
                                          {continue, CallExpr2, ArgsFold2} ->
                                              ArgsFold3 = push_expression(CallExpr2, ArgsFold2),
                                              fold(ArgsRest, ArgsFold3);
                                          {abort, _ArgsFold2} = Ret ->
                                              Ret
                                      end
                              end,
            Fold2 = push_expression(Expr1, Fold1),
            fold_inner_exprs(Args, ArgsEndCallback, Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;
fold(
  [#clause{args = Args, body = Body} = Expr | Rest],
  #fold{matching = false} = Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            BodyEndCallback = fun(Body1, BodyRest, BodyFold) ->
                                      {ClauseExpr, BodyFold1} = pop_expression(BodyFold),
                                      ClauseExpr1 = ClauseExpr#clause{body = Body1},
                                      case post_callback(ClauseExpr1, BodyFold1) of
                                          {continue, ClauseExpr2, BodyFold2} ->
                                              BodyFold3 = push_expression(ClauseExpr2, BodyFold2),
                                              fold(BodyRest, BodyFold3);
                                          {abort, _BodyFold2} = Ret ->
                                              Ret
                                      end
                              end,
            ArgsEndCallback = fun(Args1, ArgsRest, ArgsFold) ->
                                      {ClauseExpr1, ArgsFold1} = pop_expression(ArgsFold),
                                      ClauseExpr2 = ClauseExpr1#clause{args = Args1},
                                      ArgsFold2 = push_expression(ClauseExpr2, ArgsFold1),
                                      ArgsFold3 = disable_matching(ArgsFold2),
                                      fold_inner_exprs(Body, BodyEndCallback, ArgsRest, ArgsFold3)
                              end,
            Fold2 = push_expression(Expr1, Fold1),
            Fold3 = enable_matching(Fold2),
            {Rest1, Fold4} = enter_scope(Rest, Fold3),
            fold_inner_exprs(Args, ArgsEndCallback, Rest1, Fold4);
        {abort, _Fold1} = Ret ->
            Ret
    end;
fold(
  [#cons{head = Head, tail = Tail} = Expr | Rest],
  Fold) ->
    TailEndCallback = fun([Tail1], TailRest, TailFold) ->
                              {ConsExpr, TailFold1} = pop_expression(TailFold),
                              ConsExpr1 = ConsExpr#cons{tail = Tail1},
                              TailFold2 = push_expression(ConsExpr1, TailFold1),
                              fold(TailRest, TailFold2)
                      end,
    HeadEndCallback = fun([Head1], HeadRest, HeadFold) ->
                              {ConsExpr, HeadFold1} = pop_expression(HeadFold),
                              ConsExpr1 = ConsExpr#cons{head = Head1},
                              HeadFold2 = push_expression(ConsExpr1, HeadFold1),
                              fold_inner_exprs([Tail], TailEndCallback, HeadRest, HeadFold2)
                      end,
    Fold1 = push_expression(Expr, Fold),
    {Rest1, Fold2} = enter_scope(Rest, Fold1),
    fold_inner_exprs([Head], HeadEndCallback, Rest1, Fold2);
fold(
  [#eof{} = Expr | Rest],
  Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
fold(
  [#'fun'{props = {clauses, Clauses}} = Expr | Rest],
  Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            ClausesEndCallback = fun(Clauses1, ClausesRest, ClausesFold) ->
                                         {FunctionExpr, ClausesFold1} = pop_expression(ClausesFold),
                                         FunctionExpr1 = FunctionExpr#'fun'{props = {clauses, Clauses1}},
                                         case post_callback(FunctionExpr1, ClausesFold1) of
                                             {continue, FunctionExpr2, ClausesFold2} ->
                                                 ClausesFold3 = push_expression(FunctionExpr2, ClausesFold2),
                                                 fold(ClausesRest, ClausesFold3);
                                             {abort, _ClausesFold2} = Ret ->
                                                 Ret
                                         end
                                 end,
            Fold2 = push_expression(Expr1, Fold1),
            fold_inner_exprs(Clauses, ClausesEndCallback, Rest, Fold2);
        {skip, Fold1} ->
            Fold2 = push_expression(Expr, Fold1),
            fold(Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;
fold(
  [#function{clauses = Clauses} = Expr | Rest],
  Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            ClausesEndCallback = fun(Clauses1, ClausesRest, ClausesFold) ->
                                         {FunctionExpr, ClausesFold1} = pop_expression(ClausesFold),
                                         FunctionExpr1 = FunctionExpr#function{clauses = Clauses1},
                                         case post_callback(FunctionExpr1, ClausesFold1) of
                                             {continue, FunctionExpr2, ClausesFold2} ->
                                                 ClausesFold3 = push_expression(FunctionExpr2, ClausesFold2),
                                                 fold(ClausesRest, ClausesFold3);
                                             {abort, _ClausesFold2} = Ret ->
                                                 Ret
                                         end
                                 end,
            Fold2 = push_expression(Expr1, Fold1),
            fold_inner_exprs(Clauses, ClausesEndCallback, Rest, Fold2);
        {skip, Fold1} ->
            Fold2 = push_expression(Expr, Fold1),
            fold(Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;
fold(
  [#integer{} = Expr | Rest],
  Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
fold(
  [#match{left = Left, right = Right} = Expr | Rest],
  #fold{matching = false} = Fold) ->
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            RightEndCallback = fun([Right1], RightRest, RightFold) ->
                                       {MatchExpr, RightFold1} = pop_expression(RightFold),
                                       MatchExpr1 = MatchExpr#match{right = Right1},
                                       case post_callback(MatchExpr1, RightFold1) of
                                           {continue, MatchExpr2, RightFold2} ->
                                               RightFold3 = push_expression(MatchExpr2, RightFold2),
                                               fold(RightRest, RightFold3);
                                           {abort, _RightFold2} = Ret ->
                                               Ret
                                       end
                               end,
            LeftEndCallback = fun([Left1], LeftRest, LeftFold) ->
                                       {MatchExpr, LeftFold1} = pop_expression(LeftFold),
                                       MatchExpr1 = MatchExpr#match{right = Left1},
                                       LeftFold2 = push_expression(MatchExpr1, LeftFold1),
                                       LeftFold3 = disable_matching(LeftFold2),
                                       fold_inner_exprs([Right], RightEndCallback, LeftRest, LeftFold3)
                               end,
            Fold2 = push_expression(Expr1, Fold1),
            Fold3 = enable_matching(Fold2),
            fold_inner_exprs([Left], LeftEndCallback, Rest, Fold3)
    end;
fold(
  [#nil{} = Expr | Rest],
  Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
fold(
  [#op{value = Value} = Expr | Rest],
  Fold) ->
    ValueEndCallback = fun([Value1], ValueRest, ValueFold) ->
                               {OpExpr, ValueFold1} = pop_expression(ValueFold),
                               OpExpr1 = OpExpr#op{value = Value1},
                               ValueFold2 = push_expression(OpExpr1, ValueFold1),
                               fold(ValueRest, ValueFold2)
                       end,
    Fold1 = push_expression(Expr, Fold),
    fold_inner_exprs([Value], ValueEndCallback, Rest, Fold1);
fold(
  [#string{} = Expr | Rest],
  Fold) ->
    Fold1 = push_expression(Expr, Fold),
    fold(Rest, Fold1);
fold(
  [#tuple{} = Expr | Rest],
  Fold) ->
    handle_expr_with_one_inner_expr_list(Expr, #tuple.elements, Rest, Fold);
fold(
  [#var{name = Name} = Expr | Rest],
  #fold{matching = Matching, vars = Vars} = Fold) ->
    Vars1 = case Vars of
                #{Name := _} -> Vars;
                _            -> Vars#{Name => Matching}
            end,
    Fold1 = Fold#fold{vars = Vars1},
    Fold2 = push_expression(Expr, Fold1),
    fold(Rest, Fold2);

fold(
  [#scope_exit{vars = Vars} | Rest],
  Fold) ->
    Fold1 = Fold#fold{vars = Vars},
    fold(Rest, Fold1);
fold(
  [#inner_exprs_end{callback = Fun, output_exprs = OutputExprs, rest = Rest}],
  #fold{output_exprs = InnerExprs} = Fold) ->
    InnerExprs1 = lists:reverse(InnerExprs),
    Fold1 = Fold#fold{output_exprs = OutputExprs},
    Fun(InnerExprs1, Rest, Fold1);

fold(
  [],
  Fold) ->
    {ok, Fold}.

handle_expr_with_one_inner_expr(Expr, InnerId, Rest, Fold) ->
    InnerExpr = element(InnerId, Expr),
    EndCallback = fun([InnerExpr1], InnerRest, InnerFold) ->
                          {Expr1, InnerFold1} = pop_expression(InnerFold),
                          Expr2 = setelement(InnerId, Expr1, InnerExpr1),
                          InnerFold2 = push_expression(Expr2, InnerFold1),
                          fold(InnerRest, InnerFold2)
                  end,
    Fold1 = push_expression(Expr, Fold),
    fold_inner_exprs([InnerExpr], EndCallback, Rest, Fold1).

handle_expr_with_one_inner_expr_list(Expr, InnerId, Rest, Fold) ->
    InnerExprs = element(InnerId, Expr),
    EndCallback = fun(InnerExprs1, InnerRest, InnerFold) ->
                          {Expr1, InnerFold1} = pop_expression(InnerFold),
                          Expr2 = setelement(InnerId, Expr1, InnerExprs1),
                          InnerFold2 = push_expression(Expr2, InnerFold1),
                          fold(InnerRest, InnerFold2)
                  end,
    Fold1 = push_expression(Expr, Fold),
    fold_inner_exprs(InnerExprs, EndCallback, Rest, Fold1).

fold_inner_exprs(InnerExprs, EndCallback, Rest, #fold{output_exprs = OutputExprs} = Fold) ->
    EndInnerExprs = #inner_exprs_end{callback = EndCallback,
                                     output_exprs = OutputExprs,
                                     rest = Rest},
    Fold1 = Fold#fold{output_exprs = []},
    fold(InnerExprs ++ [EndInnerExprs], Fold1).

enter_scope(Rest, #fold{vars = Vars} = Fold) ->
    ExitScope = #scope_exit{vars = Vars},
    Rest1 = [ExitScope | Rest],
    {Rest1, Fold}.

push_expression(Expression, #fold{output_exprs = OutputExprs} = Fold) ->
    OutputExprs1 = [Expression | OutputExprs],
    Fold1 = Fold#fold{output_exprs = OutputExprs1},
    Fold1.

pop_expression(#fold{output_exprs = [Expression | OutputExprs]} = Fold) ->
    Fold1 = Fold#fold{output_exprs = OutputExprs},
    {Expression, Fold1}.

pre_callback(Expr, #fold{pre_callback = PreCallback} = Fold) ->
    run_callback(PreCallback, Expr, Fold).

post_callback(Expr, #fold{post_callback = PostCallback} = Fold) ->
    run_callback(PostCallback, Expr, Fold).

run_callback(none, Expr, Fold) ->
    {continue, Expr, Fold};
run_callback(Callback, Expr, #fold{vars = Vars, priv = Priv} = Fold) ->
    case Callback(Expr, Vars, Priv) of
        {Step, Priv1}
          when Step =:= continue orelse Step =:= skip orelse Step =:= abort ->
            Fold1 = Fold#fold{priv = Priv1},
            {Step, Fold1};
        {Step, Expr1, Priv1}
          when Step =:= continue orelse Step =:= skip ->
            Fold1 = Fold#fold{priv = Priv1},
            {Step, Expr1, Fold1}
    end.

enable_matching(Fold) ->
    MatchingFold = Fold#fold{matching = true},
    MatchingFold.

disable_matching(Fold) ->
    NonMatchingFold = Fold#fold{matching = false},
    NonMatchingFold.

to_erlang_code(AbstractCode) ->
    Form = erl_syntax:form_list(AbstractCode),
    SourceCode = erl_prettypr:format(Form),
    SourceCode.
