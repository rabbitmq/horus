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
         get_depth/1,
         format/1]).

-if(?OTP_RELEASE < 29).
-dialyzer({no_missing_calls, [get_inner_nodes_sets_of_c_record/1,
                              set_inner_nodes_sets_of_c_record/2,
                              get_inner_nodes_sets_of_c_record_pair/1,
                              set_inner_nodes_sets_of_c_record_pair/2]}).
-endif.

-type cerl_ctype() :: alias | apply | binary | bitstr | call | 'case' |
                      'catch' | clause | cons | 'fun' | 'let' | letrec |
                      literal | map | map_pair | module | opaque | primop |
                      'receive' | seq | record | record_pair | 'try' | tuple |
                      values | var.

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
    do_get1(Reference, Target, ModuleCoreErlang).

do_get1(Reference, Target, ModuleCoreErlang) when is_function(Reference) ->
    % ?LOG_ALERT("Module Core Erlang: ~p", [ModuleCoreErlang]),
    PreCallback = fun(Node, _Fold, undefined = Priv) ->
                          case cerl:type(Node) of
                              'fun' ->
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
                              _ ->
                                  {in, Priv}
                          end
                  end,
    case horus_cerl_utils:fold(ModuleCoreErlang, PreCallback, none, undefined) of
        {interrupted, FunCoreErlang} ->
            {ok, FunCoreErlang}
    end;
do_get1(Reference, Target, ModuleCoreErlang) when is_tuple(Reference) ->
    % ?LOG_ALERT("Module Core Erlang: ~p", [ModuleCoreErlang]),
    PreCallback = fun(Node, _Fold, undefined = Priv) ->
                          case cerl:type(Node) of
                              module ->
                                  Definitions = cerl:module_defs(Node),
                                  case find_ref(Definitions, Target) of
                                      undefined ->
                                          {in, Priv};
                                      FunCoreErlang ->
                                          {stop, FunCoreErlang}
                                  end;
                              letrec ->
                                  Definitions = cerl:letrec_defs(Node),
                                  case find_ref(Definitions, Target) of
                                      undefined ->
                                          {in, Priv};
                                      FunCoreErlang ->
                                          {stop, FunCoreErlang}
                                  end;
                              _ ->
                                  {in, Priv}
                          end
                  end,
    case horus_cerl_utils:fold(ModuleCoreErlang, PreCallback, none, undefined) of
        {interrupted, FunCoreErlang} ->
            {ok, FunCoreErlang}
    end.

find_ref([{Var, FunCoreErlang} | Rest], Target) ->
    case cerl:var_name(Var) of
        Target -> FunCoreErlang;
        _      -> find_ref(Rest, Target)
    end;
find_ref([], _Target) ->
    undefined.

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
    ?LOG_DEBUG(
       "Horus: ~*sfold/post: node '~s', ann = ~0p",
       [Depth * 2, "", cerl:type(Node), cerl:get_ann(Node)]),
    case run_post_callback(Node, Fold) of
        {ok, Node1, Fold1} ->
            OutputBuffer1 = [Node1 | OutputBuffer],
            Fold2 = Fold1#fold{output_buffer = OutputBuffer1},
            fold(Rest, Fold2);
        {abort, #fold{priv = Priv1}} ->
            {interrupted, Priv1}
    end;
fold([Node | Rest], #fold{depth = Depth} = Fold) ->
    ?LOG_DEBUG(
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
            {interrupted, Priv1}
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
            ?LOG_DEBUG(
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
    ?LOG_DEBUG(
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
    ?LOG_DEBUG(
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

-spec get_inner_nodes_sets(NodeType, Node) -> InnerNodesSets when
      NodeType :: cerl_ctype(),
      Node :: cerl:cerl(),
      InnerNodesSets :: [InnerNodesSet],
      InnerNodesSet :: [cerl:cerl()].

get_inner_nodes_sets(alias, Node) ->
    get_inner_nodes_sets_of_c_alias(Node);
get_inner_nodes_sets(apply, Node) ->
    get_inner_nodes_sets_of_c_apply(Node);
get_inner_nodes_sets(binary, Node) ->
    get_inner_nodes_sets_of_c_binary(Node);
get_inner_nodes_sets(bitstr, Node) ->
    get_inner_nodes_sets_of_c_bitstr(Node);
get_inner_nodes_sets(call, Node) ->
    get_inner_nodes_sets_of_c_call(Node);
get_inner_nodes_sets('case', Node) ->
    get_inner_nodes_sets_of_c_case(Node);
get_inner_nodes_sets('catch', Node) ->
    get_inner_nodes_sets_of_c_catch(Node);
get_inner_nodes_sets(clause, Node) ->
    get_inner_nodes_sets_of_c_clause(Node);
get_inner_nodes_sets(cons, Node) ->
    get_inner_nodes_sets_of_c_cons(Node);
get_inner_nodes_sets('fun', Node) ->
    get_inner_nodes_sets_of_c_fun(Node);
get_inner_nodes_sets(letrec, Node) ->
    get_inner_nodes_sets_of_c_letrec(Node);
get_inner_nodes_sets('let', Node) ->
    get_inner_nodes_sets_of_c_let(Node);
get_inner_nodes_sets(literal, _Node) ->
    [];
get_inner_nodes_sets(map, Node) ->
    get_inner_nodes_sets_of_c_map(Node);
get_inner_nodes_sets(map_pair, Node) ->
    get_inner_nodes_sets_of_c_map_pair(Node);
get_inner_nodes_sets(module, Node) ->
    get_inner_nodes_sets_of_c_module(Node);
get_inner_nodes_sets(opaque, _Node) ->
    [];
get_inner_nodes_sets(primop, Node) ->
    get_inner_nodes_sets_of_c_primop(Node);
get_inner_nodes_sets('receive', Node) ->
    get_inner_nodes_sets_of_c_receive(Node);
get_inner_nodes_sets(record, Node) ->
    get_inner_nodes_sets_of_c_record(Node);
get_inner_nodes_sets(record_pair, Node) ->
    get_inner_nodes_sets_of_c_record_pair(Node);
get_inner_nodes_sets(seq, Node) ->
    get_inner_nodes_sets_of_c_seq(Node);
get_inner_nodes_sets('try', Node) ->
    get_inner_nodes_sets_of_c_try(Node);
get_inner_nodes_sets(tuple, Node) ->
    get_inner_nodes_sets_of_c_tuple(Node);
get_inner_nodes_sets(values, Node) ->
    get_inner_nodes_sets_of_c_values(Node);
get_inner_nodes_sets(var, _Node) ->
    [].

set_inner_nodes_sets(alias, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_alias(Node, InnerNodesSets);
set_inner_nodes_sets(apply, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_apply(Node, InnerNodesSets);
set_inner_nodes_sets(binary, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_binary(Node, InnerNodesSets);
set_inner_nodes_sets(bitstr, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_bitstr(Node, InnerNodesSets);
set_inner_nodes_sets(call, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_call(Node, InnerNodesSets);
set_inner_nodes_sets('case', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_case(Node, InnerNodesSets);
set_inner_nodes_sets('catch', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_catch(Node, InnerNodesSets);
set_inner_nodes_sets(clause, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_clause(Node, InnerNodesSets);
set_inner_nodes_sets(cons, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_cons(Node, InnerNodesSets);
set_inner_nodes_sets('fun', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_fun(Node, InnerNodesSets);
set_inner_nodes_sets('let', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_let(Node, InnerNodesSets);
set_inner_nodes_sets(letrec, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_letrec(Node, InnerNodesSets);
set_inner_nodes_sets(map, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_map(Node, InnerNodesSets);
set_inner_nodes_sets(map_pair, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_map_pair(Node, InnerNodesSets);
set_inner_nodes_sets(module, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_module(Node, InnerNodesSets);
set_inner_nodes_sets('receive', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_receive(Node, InnerNodesSets);
set_inner_nodes_sets(record, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_record(Node, InnerNodesSets);
set_inner_nodes_sets(record_pair, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_record_pair(Node, InnerNodesSets);
set_inner_nodes_sets(primop, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_primop(Node, InnerNodesSets);
set_inner_nodes_sets(seq, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_seq(Node, InnerNodesSets);
set_inner_nodes_sets('try', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_try(Node, InnerNodesSets);
set_inner_nodes_sets(tuple, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_tuple(Node, InnerNodesSets);
set_inner_nodes_sets(values, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_values(Node, InnerNodesSets).

get_inner_nodes_sets_of_c_alias(Node) ->
    Variable = cerl:alias_var(Node),
    Pattern = cerl:alias_pat(Node),
    InnerNodesSets = [[Variable], [Pattern]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_alias(Node, [[Variable], [Pattern]]) ->
    ?assert(cerl:is_c_var(Variable)),
    Node1 = cerl:update_c_alias(Node, Variable, Pattern),
    Node1.

get_inner_nodes_sets_of_c_apply(Node) ->
    Operator = cerl:apply_op(Node),
    Arguments = cerl:apply_args(Node),
    InnerNodesSets = [[Operator], Arguments],
    InnerNodesSets.

set_inner_nodes_sets_of_c_apply(Node, [[Operator], Arguments]) ->
    ?assert(is_list(Arguments)),
    Node1 = cerl:update_c_apply(Node, Operator, Arguments),
    Node1.

get_inner_nodes_sets_of_c_binary(Node) ->
    Segments = cerl:binary_segments(Node),
    InnerNodesSets = [Segments],
    InnerNodesSets.

set_inner_nodes_sets_of_c_binary(Node, [Segments]) ->
    ?assert(is_list(Segments)),
    Node1 = cerl:update_c_binary(Node, Segments),
    Node1.

get_inner_nodes_sets_of_c_bitstr(Node) ->
    Value = cerl:bitstr_val(Node),
    Size = cerl:bitstr_size(Node),
    Unit = cerl:bitstr_unit(Node),
    Type = cerl:bitstr_type(Node),
    Flags = cerl:bitstr_flags(Node),
    InnerNodesSets = [[Value], [Size], [Unit], [Type], [Flags]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_bitstr(
  Node, [[Value], [Size], [Unit], [Type], [Flags]]) ->
    Node1 = cerl:update_c_bitstr(Node, Value, Size, Unit, Type, Flags),
    Node1.

get_inner_nodes_sets_of_c_call(Node) ->
    Module = cerl:call_module(Node),
    Name = cerl:call_name(Node),
    Arguments = cerl:call_args(Node),
    InnerNodesSets = [[Module], [Name], Arguments],
    InnerNodesSets.

set_inner_nodes_sets_of_c_call(Node, [[Module], [Name], Arguments]) ->
    ?assert(is_list(Arguments)),
    Node1 = cerl:update_c_call(Node, Module, Name, Arguments),
    Node1.

get_inner_nodes_sets_of_c_case(Node) ->
    Argument = cerl:case_arg(Node),
    Clauses = cerl:case_clauses(Node),
    InnerNodesSets = [[Argument], Clauses],
    InnerNodesSets.

set_inner_nodes_sets_of_c_case(Node, [[Argument], Clauses]) ->
    ?assert(is_list(Clauses)),
    Node1 = cerl:update_c_case(Node, Argument, Clauses),
    Node1.

get_inner_nodes_sets_of_c_catch(Node) ->
    Body = cerl:catch_body(Node),
    InnerNodesSets = [[Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_catch(Node, [[Body]]) ->
    Node1 = cerl:update_c_catch(Node, Body),
    Node1.

get_inner_nodes_sets_of_c_clause(Node) ->
    Patterns = cerl:clause_pats(Node),
    Body = cerl:clause_body(Node),
    InnerNodesSets = [[{matching, true} | Patterns],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_clause(Node, [Patterns, [Body]]) ->
    ?assert(is_list(Patterns)),
    Guard = cerl:clause_guard(Node),
    Node1 = cerl:update_c_clause(Node, Patterns, Guard, Body),
    Node1.

get_inner_nodes_sets_of_c_cons(Node) ->
    Head = cerl:cons_hd(Node),
    Tail = cerl:cons_tl(Node),
    InnerNodesSets = [[Head], [Tail]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_cons(Node, [[Head], [Tail]]) ->
    Node1 = cerl:update_c_cons(Node, Head, Tail),
    Node1.

get_inner_nodes_sets_of_c_fun(Node) ->
    Arguments = cerl:fun_vars(Node),
    Body = cerl:fun_body(Node),
    InnerNodesSets = [[{matching, true} | Arguments],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_fun(Node, [Arguments, [Body]]) ->
    ?assert(is_list(Arguments)),
    Node1 = cerl:update_c_fun(Node, Arguments, Body),
    Node1.

get_inner_nodes_sets_of_c_let(Node) ->
    Variables = cerl:let_vars(Node),
    Argument = cerl:let_arg(Node),
    Body = cerl:let_body(Node),
    InnerNodesSets = [[{matching, true} | Variables],
                      [{matching, false}, Argument],
                      [{matching, false}, Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_let(Node, [Variables, [Argument], [Body]]) ->
    ?assert(is_list(Variables)),
    Node1 = cerl:update_c_let(Node, Variables, Argument, Body),
    Node1.

get_inner_nodes_sets_of_c_letrec(Node) ->
    Definitions = cerl:letrec_defs(Node),
    FunNodes = [FunNode || {_Var, FunNode} <- Definitions],
    Body = cerl:letrec_body(Node),
    InnerNodesSets = [FunNodes, [Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_letrec(Node, [FunNodes, [Body]]) ->
    ?assert(is_list(FunNodes)),
    Definitions1 = cerl:letrec_defs(Node),
    Definitions2 = update_defs(Definitions1, FunNodes),
    Node1 = cerl:update_c_letrec(Node, Definitions2, Body),
    Node1.

get_inner_nodes_sets_of_c_map(Node) ->
    Map = cerl:map_arg(Node),
    Pairs  = cerl:map_es(Node),
    InnerNodesSets = [[Map], Pairs],
    InnerNodesSets.

set_inner_nodes_sets_of_c_map(Node, [[Map], Pairs]) ->
    ?assert(is_list(Pairs)),
    Node1 = cerl:update_c_map(Node, Map, Pairs),
    Node1.

get_inner_nodes_sets_of_c_map_pair(Node) ->
    Key = cerl:map_pair_key(Node),
    Value  = cerl:map_pair_val(Node),
    InnerNodesSets = [[Key], [Value]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_map_pair(Node, [[Key], [Value]]) ->
    Node1 = cerl:update_c_map_pair(
              Node,
              cerl:map_pair_op(Node),
              Key,
              Value),
    Node1.

get_inner_nodes_sets_of_c_module(Node) ->
    Definitions = cerl:module_defs(Node),
    FunNodes = [FunNode || {_Var, FunNode} <- Definitions],
    InnerNodesSets = [FunNodes],
    InnerNodesSets.

set_inner_nodes_sets_of_c_module(Node, [FunNodes]) ->
    % Definitions = [begin
    %                    Ann = cerl:get_ann(FunNode),
    %                    {function, FunName} = lists:keyfind(function, 1, Ann),
    %                    NameNode = cerl:c_var(FunName),
    %                    FunDef = {NameNode, FunNode},
    %                    FunDef
    %                end || FunNode <- FunNodes],
    Definitions1 = cerl:module_defs(Node),
    Definitions2 = update_defs(Definitions1, FunNodes),
    Node1 = cerl:update_c_module(
              Node,
              cerl:module_name(Node),
              cerl:module_exports(Node),
              cerl:module_attrs(Node),
              Definitions2),
    Node1.

get_inner_nodes_sets_of_c_primop(Node) ->
    Name = cerl:primop_name(Node),
    Arguments  = cerl:primop_args(Node),
    InnerNodesSets = [[Name], Arguments],
    InnerNodesSets.

set_inner_nodes_sets_of_c_primop(Node, [[Name], Arguments]) ->
    Node1 = cerl:update_c_primop(
              Node,
              Name,
              Arguments),
    Node1.

get_inner_nodes_sets_of_c_receive(Node) ->
    Clauses = cerl:receive_clauses(Node),
    Timeout = cerl:receive_timeout(Node),
    Action = cerl:receive_action(Node),
    InnerNodesSets = [Clauses, [Timeout], [Action]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_receive(Node, [Clauses, [Timeout], [Action]]) ->
    ?assert(is_list(Clauses)),
    Node1 = cerl:update_c_receive(
              Node,
              Clauses,
              Timeout,
              Action),
    Node1.

get_inner_nodes_sets_of_c_record(Node) ->
    Pairs = cerl:record_es(Node),
    InnerNodesSets = [Pairs],
    InnerNodesSets.

set_inner_nodes_sets_of_c_record(Node, [Pairs]) ->
    Node1 = cerl:update_c_record(
              Node,
              cerl:record_arg(Node),
              cerl:record_id(Node),
              Pairs),
    Node1.

get_inner_nodes_sets_of_c_record_pair(Node) ->
    Value  = cerl:record_pair_val(Node),
    InnerNodesSets = [[Value]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_record_pair(Node, [[Value]]) ->
    Node1 = cerl:update_c_record_pair(
              Node,
              cerl:record_pair_key(Node),
              Value),
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

get_inner_nodes_sets_of_c_try(Node) ->
    Argument = cerl:try_arg(Node),
    Vars = cerl:try_vars(Node),
    Body = cerl:try_body(Node),
    ExceptionVars = cerl:try_evars(Node),
    Handler = cerl:try_handler(Node),
    InnerNodesSets = [[{matching, false}, Argument],
                      [{matching, true} | Vars],
                      [{matching, false}, Body],
                      [{matching, true} | ExceptionVars],
                      [{matching, false}, Handler]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_try(Node, [[Argument], Vars, [Body], ExceptionVars, [Handler]]) ->
    Node1 = cerl:update_c_try(
              Node,
              Argument,
              Vars,
              Body,
              ExceptionVars,
              Handler),
    Node1.

get_inner_nodes_sets_of_c_tuple(Node) ->
    Elements = cerl:tuple_es(Node),
    InnerNodesSets = [Elements],
    InnerNodesSets.

set_inner_nodes_sets_of_c_tuple(Node, [Elements]) ->
    Node1 = cerl:update_c_tuple(
              Node,
              Elements),
    Node1.

get_inner_nodes_sets_of_c_values(Node) ->
    Elements = cerl:values_es(Node),
    InnerNodesSets = [Elements],
    InnerNodesSets.

set_inner_nodes_sets_of_c_values(Node, [Elements]) ->
    Node1 = cerl:update_c_values(
              Node,
              Elements),
    Node1.

update_defs(Definitions, FunNodes) ->
    update_defs(Definitions, FunNodes, []).

update_defs([{Var, _FunNode} | Rest1], [FunNode | Rest2], Acc) ->
    Acc1 = [{Var, FunNode} | Acc],
    update_defs(Rest1, Rest2, Acc1);
update_defs([], [], Acc) ->
    lists:reverse(Acc).

get_matching(#fold{matching = Matching}) ->
    Matching.

get_depth(#fold{depth = Depth}) ->
    Depth.

format(CoreErlang) ->
    Txt = core_pp:format(CoreErlang),
    io:format("~s~n", [Txt]).
