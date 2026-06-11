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
                             #clauses{} |
                             #'fun'{} |
                             #function{} |
                             #integer{} |
                             #match{} |
                             #op{} |
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
                      (Expr, _Vars, Priv) ->
                          {skip, Expr, Priv}
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

fold([Expr | Rest], Fold)
  when is_record(Expr, atom) orelse
       is_record(Expr, eof) orelse
       is_record(Expr, integer) orelse
       is_record(Expr, nil) orelse
       is_record(Expr, string) ->
    handle_expr(Expr, [], false, Rest, Fold);
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
fold([#call{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #call.args,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, true, Rest, Fold);
fold([#clause{} = Expr | Rest], #fold{matching = false} = Fold) ->
    InnerExprs = [#inner_exprs_start{index = #clause.args,
                                     matching = true},
                  #inner_exprs_start{index = #clause.body,
                                     matching = false}],
    {Rest1, Fold1} = enter_scope(Rest, Fold),
    handle_expr(Expr, InnerExprs, true, Rest1, Fold1);
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
fold([#match{} = Expr | Rest], #fold{matching = false} = Fold) ->
    InnerExprs = [#inner_exprs_start{index = #match.left,
                                     matching = true},
                  #inner_exprs_start{index = #match.right,
                                     matching = false}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#op{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #op.value,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold([#tuple{} = Expr | Rest], Fold) ->
    InnerExprs = [#inner_exprs_start{index = #tuple.elements,
                                     matching = inherit}],
    handle_expr(Expr, InnerExprs, false, Rest, Fold);
fold(
  [#var{name = Name} = Expr | Rest],
  #fold{matching = Matching, vars = Vars} = Fold) ->
    Vars1 = case Vars of
                #{Name := _} -> Vars;
                _            -> Vars#{Name => Matching}
            end,
    Fold1 = Fold#fold{vars = Vars1},
    handle_expr(Expr, [], false, Rest, Fold1);

fold(
  [#inner_exprs_start{index = Index, matching = Matching} | Rest],
  #fold{matching = OldMatching, output_exprs = [ParentExpr | _] = OutputExprs} = Fold) ->
    InnerExprs = element(Index, ParentExpr),
    IsList = is_list(InnerExprs),
    InnerExprsEnd = #inner_exprs_end{index = Index,
                                     is_list = IsList,
                                     old_matching = OldMatching,
                                     output_exprs = OutputExprs},
    Fold1 = Fold#fold{output_exprs = []},
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
  #fold{output_exprs = InnerExprs} = Fold) ->
    InnerExprs1 = case IsList of
                      true ->
                          lists:reverse(InnerExprs);
                      false ->
                          [InnerExpr] = InnerExprs,
                          InnerExpr
                  end,
    ParentExpr1 = setelement(Index, ParentExpr, InnerExprs1),
    OutputExprs1 = [ParentExpr1 | OutputExprs],
    Fold1 = Fold#fold{output_exprs = OutputExprs1},
    Fold2 = set_matching(OldMatching, Fold1),
    fold(Rest, Fold2);
fold(
  [#scope_exit{vars = Vars} | Rest],
  Fold) ->
    Fold1 = Fold#fold{vars = Vars},
    fold(Rest, Fold1);
fold(
  [post_callback | Rest],
  #fold{output_exprs = [Expr | OutputExprs]} = Fold) ->
    case post_callback(Expr, Fold) of
        {Step, Expr1, Fold1} when Step =:= continue orelse Step =:= skip ->
            OutputExprs1 = [Expr1 | OutputExprs],
            Fold2 = Fold1#fold{output_exprs = OutputExprs1},
            fold(Rest, Fold2);
        {abort, _Fold1} = Ret ->
            Ret
    end;

fold(
  [],
  Fold) ->
    {ok, Fold}.

handle_expr(Expr, InnerExprIdxs, true = _UseCallbacks, Rest, Fold) ->
    Rest1 = [post_callback | Rest],
    case pre_callback(Expr, Fold) of
        {continue, Expr1, Fold1} ->
            do_handle_expr(Expr1, InnerExprIdxs, Rest1, Fold1);
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

enter_scope(Rest, #fold{vars = Vars} = Fold) ->
    ExitScope = #scope_exit{vars = Vars},
    Rest1 = [ExitScope | Rest],
    {Rest1, Fold}.

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

to_erlang_code(AbstractCode) ->
    Form = erl_syntax:form_list(AbstractCode),
    SourceCode = erl_prettypr:format(Form),
    SourceCode.
