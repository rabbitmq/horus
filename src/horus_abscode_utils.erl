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
-export([get_expr_depth/1,
         get_vars/1]).

-type vars_map() :: #{atom() => boolean()}.

%% `fold/5' state.

-record(fold, {matching = false :: boolean(),
               vars = #{} :: horus_abscode_utils:vars_map(),
               expr_depth = 1,
               output_exprs = [],

               source,
               pre_callback :: fun(),
               post_callback :: fun(),
               priv :: any()}).

-opaque fold_state() :: #fold{}.

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
    %% FIXME: `StartLine' not enough. Use the first asm instruction and its
    %% line number to determine which function on that same line is the best
    %% match.
    {StartLine, FirstExprLine, _FirstInstruction} = horus_asm_utils:get_fun_start_lines(Fun),
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code(Beam),
    % logger:alert("Module = ~p~nFunInfo = ~p~nStartLine = ~b~nFirstInstruction = ~0p @ ~b", [AbstractCode, FunInfo, StartLine, _FirstInstruction, FirstExprLine]),
    %% FIXME: Collect all variables from args and in the function body from the start
    %% of the function up to the definition of the `Fun' when they are
    %% deterministic. Args in parent `fun()' overrides previous variables with
    %% the same name.
    %%
    %% Use that to copy variable definitions to the beginning of the extracted
    %% function.
    PreCallback = fun
                      (#attribute{name = file} = Expr,
                       _Fold, Priv) ->
                          Priv1 = Priv#{source_file => Expr},
                          {continue, Expr, Priv1};
                      (#'fun'{location = {StartLine1, _StartCol},
                              code = #clauses{clauses = Clauses}} = Expr,
                       _Fold, #{scopes := [Scope | _]} = Priv)
                        when not is_map_key(abstract_code, Priv) andalso
                             StartLine1 =:= StartLine ->
                          FirstExpr = find_first_executable_expr(Clauses),
                          % logger:alert("Firt executable expr = ~p", [FirstExpr]),
                          case element(2, FirstExpr) of
                              {FirstExprLine, _} ->
                                  Scope1 = lists:reverse(Scope),
                                  Priv1 = Priv#{abstract_code => Expr,
                                                scope => Scope1},
                                  {abort, Priv1};
                              _ ->
                                  {continue, Expr, Priv}
                          end;
                      (enter_scope = Expr, _Fold, #{scopes := Scopes} = Priv) ->
                          Scopes1 = case Scopes of
                                        []         -> [[]];
                                        [Vars | _] -> [Vars | Scopes]
                                    end,
                          Priv1 = Priv#{scopes => Scopes1},
                          {continue, Expr, Priv1};
                      (Expr, _Fold, Priv) ->
                          {continue, Expr, Priv}
                  end,
    PostCallback = fun
                      (#match{left = #var{}} = Expr, _Fold, #{scopes := [CurrentScope | Scopes]} = Priv) ->
                          CurrentScope1 = [Expr | CurrentScope],
                          Priv1 = Priv#{scopes => [CurrentScope1 | Scopes]},
                          {continue, Expr, Priv1};
                      (#match{} = Expr, _Fold, Priv) ->
                          {continue, Expr, Priv};
                      (exit_scope = Expr, _Fold, #{scopes := Scopes} = Priv) ->
                           Scopes1 = tl(Scopes),
                           Priv1 = Priv#{scopes => Scopes1},
                          {continue, Expr, Priv1};
                       (Expr, _Fold, Priv) ->
                           {continue, Expr, Priv}
                   end,
    case fold(AbstractCode, Fun, PreCallback, PostCallback, #{scopes => []}) of
        {ok, _, #{source_file := SourceFile, abstract_code := AbstractCode1, scope := Scope}} ->
            % logger:alert("Vars in scope for ~s: ~0p", [maps:get(name, FunInfo), Scope]),
            AbstractCode2 = [SourceFile, AbstractCode1],
            {ok, AbstractCode2, Scope};
        {ok, _, #{abstract_code := AbstractCode1}} ->
            AbstractCode2 = [AbstractCode1],
            {ok, AbstractCode2};
        {ok, _, _Priv} ->
            Reason = ?horus_error(
                        fun_not_found,
                        #{function => Fun,
                          fun_info => FunInfo,
                          start_line => StartLine,
                          module_abstract_code => AbstractCode}),
            {error, Reason}
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

find_first_executable_expr([Expr | _])
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
    Expr;
find_first_executable_expr([#block{expressions = Exprs} | _]) ->
    find_first_executable_expr(Exprs);
find_first_executable_expr([#clause{body = Body} | _]) ->
    find_first_executable_expr(Body);
find_first_executable_expr([#clauses{clauses = Clauses} | _]) ->
    find_first_executable_expr(Clauses);
find_first_executable_expr([#'if'{clauses = Clauses} | _]) ->
    find_first_executable_expr(Clauses);
find_first_executable_expr([#'receive'{clauses = Clauses} | _]) ->
    find_first_executable_expr(Clauses);
find_first_executable_expr([#'try'{block = Block} | _]) ->
    find_first_executable_expr(Block).

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

enter_scope(Expr, InnerExprs, UseCallbacks, Rest, #fold{vars = Vars} = Fold) ->
    % logger:alert("ENTER SCOPE, Expr = ~p", [Expr]),
    ExitScope = #scope_exit{vars = Vars},
    Rest1 = [ExitScope | Rest],
    case pre_callback(enter_scope, Fold) of
        {Step, enter_scope, Fold1}
          when Step =:= continue orelse Step =:= skip ->
            handle_expr(Expr, InnerExprs, UseCallbacks, Rest1, Fold1);
        {abort, _Fold1} = Ret ->
            Ret
    end.

exit_scope(#scope_exit{vars = Vars}, Rest, Fold) ->
    % logger:alert("EXIT SCOPE, Rest = ~p", [hd(Rest)]),
    Fold1 = Fold#fold{vars = Vars},
    case post_callback(exit_scope, Fold1) of
        {Step, exit_scope, Fold2}
          when Step =:= continue orelse Step =:= skip ->
            fold(Rest, Fold2);
        {abort, _Fold2} = Ret ->
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

get_expr_depth(#fold{expr_depth = ExprDepth}) ->
    ExprDepth.

get_vars(#fold{vars = Vars}) ->
    Vars.

to_erlang_code(AbstractCode) ->
    Form = erl_syntax:form_list(AbstractCode),
    SourceCode = erl_prettypr:format(Form),
    SourceCode.
