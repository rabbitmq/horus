%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_cerl_utils).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([get/1,
         fold/4,
         get_matching/1,
         get_depth/1]).

-record(fold, {pre_callback,
               post_callback,
               priv,

               output_buffer = [],
               matching = false,
               depth = 0}).

get(Fun) when is_function(Fun) ->
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module,
      name := Name,
      arity := Arity} = FunInfo,
    AbstractCode = horus_beam_utils:get_abstract_code(Module),
    do_get(Fun, {Name, Arity}, AbstractCode);
get({Module, Name, Arity} = MFA) ->
    AbstractCode = horus_beam_utils:get_abstract_code(Module),
    do_get(MFA, {Name, Arity}, AbstractCode).

do_get(Reference, Target, AbstractCode) ->
    CompilerOptions = [binary,
                       to_core,
                       warnings_as_errors,
                       return_errors,
                       return_warnings,
                       deterministic],
    {ok, _, ModuleCoreErlang, _} = compile:forms(
                                     AbstractCode, CompilerOptions),
    % ?LOG_ALERT("Module Core Erlang: ~p", [ModuleCoreErlang]),
    PreCallback = fun(Node, _Fold, undefined = Priv) ->
                          case cerl:type(Node) of
                              'fun' when is_function(Reference) ->
                                  Ann = cerl:get_ann(Node),
                                  case lists:keyfind(id, 1, Ann) of
                                      {id, {_, _, ThisName}} ->
                                          ThisArity = cerl:fun_arity(Node),
                                          case {ThisName, ThisArity} of
                                              Target -> {stop, Node};
                                              _      -> {in, Priv}
                                          end;
                                      false ->
                                          {in, Priv}
                                  end;
                              'fun' when is_tuple(Reference) ->
                                  Ann = cerl:get_ann(Node),
                                  case lists:keyfind(function, 1, Ann) of
                                      {function, FunName} ->
                                          case FunName of
                                              Target -> {stop, Node};
                                              _      -> {in, Priv}
                                          end;
                                      false ->
                                          {in, Priv}
                                  end;
                              _ ->
                                  {in, Priv}
                          end
                  end,
    FunCoreErlang = horus_cerl_utils:fold(ModuleCoreErlang, PreCallback, none, undefined),
    {ok, FunCoreErlang}.

fold(Node, PreCallback, PostCallback, Priv) ->
    Fold = #fold{pre_callback = PreCallback,
                 post_callback = PostCallback,
                 priv = Priv},
    fold([Node], Fold).

fold([{matching, Matching} | Rest], Fold) ->
    Fold1 = Fold#fold{matching = Matching},
    fold(Rest, Fold1);
fold([{more_inner_nodes, InputInnerNodesSets, OutputInnerNodesSets, OutputBuffer} | Rest], Fold) ->
    fold_more_inner_nodes(InputInnerNodesSets, OutputInnerNodesSets, OutputBuffer, Rest, Fold);
fold([post | Rest], #fold{output_buffer = [Node | OutputBuffer], depth = Depth} = Fold) ->
    ?LOG_ALERT(
       "Horus: ~*sfold/post: node '~s', ann = ~0p",
       [Depth * 2, "", cerl:type(Node), cerl:get_ann(Node)]),
    case run_post_callback(Node, Fold) of
        {ok, Node1, Fold1} ->
            OutputBuffer1 = [Node1 | OutputBuffer],
            Fold2 = Fold1#fold{output_buffer = OutputBuffer1},
            fold(Rest, Fold2);
        {abort, #fold{priv = Priv1}} ->
            Priv1
    end;
fold([Node | Rest], #fold{depth = Depth} = Fold) ->
    ?LOG_ALERT(
       "Horus: ~*sfold/pre: node '~s', ann = ~0p",
       [Depth * 2, "", cerl:type(Node), cerl:get_ann(Node)]),
    case run_pre_callback(Node, Fold) of
        {in, Node1, Fold1} ->
            Rest1 = [post | Rest],
            Fold2 = add_to_output(Node1, Fold1),
            fold_inner_nodes(Node1, Rest1, Fold2);
        {next, Node1, Fold1} ->
            Rest1 = [post | Rest],
            Fold2 = add_to_output(Node1, Fold1),
            fold(Rest1, Fold2);
        {stop, #fold{priv = Priv1}} ->
            Priv1
    end;
fold([], #fold{output_buffer = [OutputNode], priv = Priv}) ->
    {ok, OutputNode, Priv}.

add_to_output(Node, #fold{output_buffer = OutputBuffer} = Fold) ->
    OutputBuffer1 = [Node | OutputBuffer],
    Fold1 = Fold#fold{output_buffer = OutputBuffer1},
    Fold1.

run_pre_callback(Node, #fold{pre_callback = none} = Fold) ->
    {in, Node, Fold};
run_pre_callback(Node, #fold{pre_callback = PreCallback, priv = Priv} = Fold) ->
    case PreCallback(Node, Fold, Priv) of
        {Next, Priv1} when Next =:= in orelse Next =:= next ->
            Fold1 = Fold#fold{priv = Priv1},
            {Next, Node, Fold1};
        {Next, Node1, Priv1} when Next =:= in orelse Next =:= next ->
            Fold1 = Fold#fold{priv = Priv1},
            {Next, Node1, Fold1};
        {stop = Next, Priv1} ->
            Fold1 = Fold#fold{priv = Priv1},
            {Next, Fold1}
    end.

run_post_callback(Node, #fold{post_callback = none} = Fold) ->
    {ok, Node, Fold};
run_post_callback(Node, #fold{post_callback = PostCallback, priv = Priv} = Fold) ->
    case PostCallback(Node, Fold, Priv) of
        {ok, Priv1} ->
            Fold1 = Fold#fold{priv = Priv1},
            {ok, Node, Fold1};
        {ok, Node1, Priv1} ->
            Fold1 = Fold#fold{priv = Priv1},
            {ok, Node1, Fold1};
        {abort, Priv1} ->
            Fold1 = Fold#fold{priv = Priv1},
            {abort, Fold1}
    end.

fold_inner_nodes(
  Node, Rest,
  #fold{output_buffer = OutputBuffer, depth = Depth} = Fold) ->
    NodeType = cerl:type(Node),
    InnerNodesSets = get_inner_nodes_sets(NodeType, Node),
    case InnerNodesSets of
        [] ->
            fold(Rest, Fold);
        [FirstInnerNodesSet | OtherInnerNodesSets] ->
            ?LOG_ALERT(
               "Horus: ~*sfold/start inner nodes sets: node '~s', 0/~b inner nodes sets handled",
               [Depth * 2, "", cerl:type(Node), length(InnerNodesSets)]),
            Rest1 = [{more_inner_nodes, OtherInnerNodesSets, [], OutputBuffer} | Rest],
            Rest2 = FirstInnerNodesSet ++ Rest1,
            InnerNodes = [],
            Fold1 = Fold#fold{output_buffer = InnerNodes,
                              depth = Depth + 1},
            fold(Rest2, Fold1)
    end.

fold_more_inner_nodes(
  [NextInnerNodesSet | OtherInnerNodesSets], OutputInnerNodesSets,
  [Node | _] = OutputBuffer,
  Rest, #fold{output_buffer = OutputInnerNodesSet, depth = Depth} = Fold) ->
    ?LOG_ALERT(
       "Horus: ~*sfold/continue inner nodes sets: node '~s', ~b/~b inner nodes sets handled",
       [Depth * 2, "", cerl:type(Node), length(OutputInnerNodesSets) + 1, length(OutputInnerNodesSets) + 1 + length(OtherInnerNodesSets) + 1]),
    OutputInnerNodesSet1 = lists:reverse(OutputInnerNodesSet),
    OutputInnerNodesSets1 = [OutputInnerNodesSet1 | OutputInnerNodesSets],
    Rest1 = [{more_inner_nodes, OtherInnerNodesSets, OutputInnerNodesSets1, OutputBuffer} | Rest],
    Rest2 = NextInnerNodesSet ++ Rest1,
    Fold1 = Fold#fold{output_buffer = []},
    fold(Rest2, Fold1);
fold_more_inner_nodes(
  [], OutputInnerNodesSets,
  [Node | OutputBuffer],
  Rest, #fold{output_buffer = OutputInnerNodesSet, depth = Depth} = Fold) ->
    ?LOG_ALERT(
       "Horus: ~*sfold/finish inner nodes sets: node '~s', ~b inner nodes sets",
       [(Depth - 1) * 2, "", cerl:type(Node), length(OutputInnerNodesSets) + 1]),
    OutputInnerNodesSet1 = lists:reverse(OutputInnerNodesSet),
    OutputInnerNodesSets1 = [OutputInnerNodesSet1 | OutputInnerNodesSets],
    OutputInnerNodesSets2 = lists:reverse(OutputInnerNodesSets1),
    NodeType = cerl:type(Node),
    Node1 = set_inner_nodes_sets(NodeType, Node, OutputInnerNodesSets2),
    Fold1 = Fold#fold{output_buffer = [Node1 | OutputBuffer],
                      depth = Depth - 1},
    fold(Rest, Fold1).

get_inner_nodes_sets(alias, Node) ->
    get_inner_nodes_sets_of_c_alias(Node);
get_inner_nodes_sets(apply, Node) ->
    get_inner_nodes_sets_of_c_apply(Node);
get_inner_nodes_sets('case', Node) ->
    get_inner_nodes_sets_of_c_case(Node);
get_inner_nodes_sets(clause, Node) ->
    get_inner_nodes_sets_of_c_clause(Node);
get_inner_nodes_sets('fun', Node) ->
    get_inner_nodes_sets_of_c_fun(Node);
get_inner_nodes_sets('let', Node) ->
    get_inner_nodes_sets_of_c_let(Node);
get_inner_nodes_sets(module, Node) ->
    get_inner_nodes_sets_of_c_module(Node);
get_inner_nodes_sets(seq, Node) ->
    get_inner_nodes_sets_of_c_seq(Node);
get_inner_nodes_sets(_NodeType, _Node) ->
    [].

set_inner_nodes_sets(alias, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_alias(Node, InnerNodesSets);
set_inner_nodes_sets(apply, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_apply(Node, InnerNodesSets);
set_inner_nodes_sets('case', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_case(Node, InnerNodesSets);
set_inner_nodes_sets(clause, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_clause(Node, InnerNodesSets);
set_inner_nodes_sets('fun', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_fun(Node, InnerNodesSets);
set_inner_nodes_sets('let', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_let(Node, InnerNodesSets);
set_inner_nodes_sets(module, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_module(Node, InnerNodesSets);
set_inner_nodes_sets(seq, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_seq(Node, InnerNodesSets).

get_inner_nodes_sets_of_c_alias(Node) ->
    Var = cerl:alias_var(Node),
    Pattern = cerl:alias_pat(Node),
    InnerNodesSets = [[Var], [Pattern]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_alias(Node, [[Var], [Pattern]]) ->
    Node1 = cerl:update_c_alias(
              Node,
              Var,
              Pattern),
    Node1.

get_inner_nodes_sets_of_c_apply(Node) ->
    Op = cerl:apply_op(Node),
    Args = cerl:apply_args(Node),
    InnerNodesSets = [[Op], Args],
    InnerNodesSets.

set_inner_nodes_sets_of_c_apply(Node, [[Op], Args]) ->
    Node1 = cerl:update_c_apply(
              Node,
              Op,
              Args),
    Node1.

get_inner_nodes_sets_of_c_case(Node) ->
    Arg = cerl:case_arg(Node),
    Clauses = cerl:case_clauses(Node),
    InnerNodesSets = [[Arg], Clauses],
    InnerNodesSets.

set_inner_nodes_sets_of_c_case(Node, [[Arg], Clauses]) ->
    Node1 = cerl:update_c_case(
              Node,
              Arg,
              Clauses),
    Node1.

get_inner_nodes_sets_of_c_clause(Node) ->
    Patterns = cerl:clause_pats(Node),
    Body = cerl:clause_body(Node),
    InnerNodesSets = [[{matching, true} | Patterns],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_clause(Node, [Patterns, [Body]]) ->
    Node1 = cerl:update_c_clause(
              Node,
              Patterns,
              cerl:clause_guard(Node),
              Body),
    Node1.

get_inner_nodes_sets_of_c_fun(Node) ->
    Args = cerl:fun_vars(Node),
    Body = cerl:fun_body(Node),
    InnerNodesSets = [[{matching, true} | Args],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_fun(Node, [Args, [Body]]) ->
    Node1 = cerl:update_c_fun(
              Node,
              Args,
              Body),
    Node1.

get_inner_nodes_sets_of_c_let(Node) ->
    Argument = cerl:let_arg(Node),
    Body = cerl:let_body(Node),
    InnerNodesSets = [[{matching, true}, Argument],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_let(Node, [[Argument], [Body]]) ->
    Node1 = cerl:update_c_let(
              Node,
              cerl:let_vars(Node),
              Argument,
              Body),
    Node1.

get_inner_nodes_sets_of_c_module(Node) ->
    ModuleDefs = cerl:module_defs(Node),
    FunDefs = [FunDef || {_, FunDef} <- ModuleDefs],
    InnerNodesSets = [FunDefs],
    InnerNodesSets.

set_inner_nodes_sets_of_c_module(Node, [FunNodes]) ->
    ModuleDefs = [begin
                      Ann = cerl:get_ann(FunNode),
                      {function, FunName} = lists:keyfind(function, 1, Ann),
                      NameNode = cerl:c_var(FunName),
                      FunDef = {NameNode, FunNode},
                      FunDef
                  end || FunNode <- FunNodes],
    Node1 = cerl:update_c_module(
              Node,
              cerl:module_name(Node),
              cerl:module_exports(Node),
              cerl:module_attrs(Node),
              ModuleDefs),
    Node1.

get_inner_nodes_sets_of_c_seq(Node) ->
    Argument = cerl:seq_arg(Node),
    Body = cerl:seq_body(Node),
    InnerNodesSets = [[Argument], [Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_seq(Node, [[Argument], [Body]]) ->
    Node1 = cerl:update_c_seq(
              Node,
              Argument,
              Body),
    Node1.

get_matching(#fold{matching = Matching}) ->
    Matching.

get_depth(#fold{depth = Depth}) ->
    Depth.
