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

-export([fold/4]).

-record(fold, {pre_callback,
               post_callback,
               priv,

               output_buffer = []}).

fold(Node, PreCallback, PostCallback, Priv) ->
    Fold = #fold{pre_callback = PreCallback,
                 post_callback = PostCallback,
                 priv = Priv},
    fold([Node], Fold).

fold([{more_inner_nodes, InputInnerNodesSets, OutputInnerNodesSets, OutputBuffer} | Rest], Fold) ->
    fold_more_inner_nodes(InputInnerNodesSets, OutputInnerNodesSets, OutputBuffer, Rest, Fold);
fold([post | Rest], #fold{output_buffer = [Node | OutputBuffer]} = Fold) ->
    ?LOG_ALERT("FOLD/post: Node=~p", [Node]),
    case run_post_callback(Node, Fold) of
        {ok, Node1, Fold1} ->
            OutputBuffer1 = [Node1 | OutputBuffer],
            Fold2 = Fold1#fold{output_buffer = OutputBuffer1},
            fold(Rest, Fold2);
        {abort, #fold{priv = Priv1}} ->
            Priv1
    end;
fold([Node | Rest], Fold) ->
    ?LOG_ALERT("FOLD/pre: Node=~p", [Node]),
    case run_pre_callback(Node, Fold) of
        {in, Fold1} ->
            Rest1 = [post | Rest],
            Fold2 = add_to_output(Node, Fold1),
            fold_inner_nodes(Node, Rest1, Fold2);
        {next, Fold1} ->
            Rest1 = [post | Rest],
            Fold2 = add_to_output(Node, Fold1),
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

run_pre_callback(_Node, #fold{pre_callback = none} = Fold) ->
    {in, Fold};
run_pre_callback(Node, #fold{pre_callback = PreCallback, priv = Priv} = Fold) ->
    {Next, Priv1} = PreCallback(Node, Fold, Priv),
    Fold1 = Fold#fold{priv = Priv1},
    {Next, Fold1}.

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

fold_inner_nodes(Node, Rest, #fold{output_buffer = OutputBuffer} = Fold) ->
    NodeType = cerl:type(Node),
    InnerNodesSets = get_inner_nodes_sets(NodeType, Node),
    case InnerNodesSets of
        [] ->
            fold(Rest, Fold);
        [FirstInnerNodesSet | OtherInnerNodesSets] ->
            ?LOG_ALERT("FOLD/enter: Node=~p", [Node]),
            Rest1 = [{more_inner_nodes, OtherInnerNodesSets, [], OutputBuffer} | Rest],
            Rest2 = FirstInnerNodesSet ++ Rest1,
            InnerNodes = [],
            Fold1 = Fold#fold{output_buffer = InnerNodes},
            fold(Rest2, Fold1)
    end.

fold_more_inner_nodes(
  [NextInnerNodesSet | OtherInnerNodesSets], OutputInnerNodesSets,
  OutputBuffer,
  Rest, #fold{output_buffer = OutputInnerNodesSet} = Fold) ->
    OutputInnerNodesSet1 = lists:reverse(OutputInnerNodesSet),
    OutputInnerNodesSets1 = [OutputInnerNodesSet1 | OutputInnerNodesSets],
    Rest1 = [{more_inner_nodes, OtherInnerNodesSets, OutputInnerNodesSets1, OutputBuffer} | Rest],
    Rest2 = NextInnerNodesSet ++ Rest1,
    Fold1 = Fold#fold{output_buffer = []},
    fold(Rest2, Fold1);
fold_more_inner_nodes(
  [], OutputInnerNodesSets,
  [Node | OutputBuffer],
  Rest, #fold{output_buffer = OutputInnerNodesSet} = Fold) ->
    ?LOG_ALERT("FOLD/exit: Node=~p", [Node]),
    OutputInnerNodesSet1 = lists:reverse(OutputInnerNodesSet),
    OutputInnerNodesSets1 = [OutputInnerNodesSet1 | OutputInnerNodesSets],
    OutputInnerNodesSets2 = lists:reverse(OutputInnerNodesSets1),
    NodeType = cerl:type(Node),
    Node1 = set_inner_nodes_sets(NodeType, Node, OutputInnerNodesSets2),
    Fold1 = Fold#fold{output_buffer = [Node1 | OutputBuffer]},
    fold(Rest, Fold1).

get_inner_nodes_sets(module, Node) ->
    get_inner_nodes_sets_of_c_module(Node);
get_inner_nodes_sets('fun', Node) ->
    get_inner_nodes_sets_of_c_fun(Node);
get_inner_nodes_sets('let', Node) ->
    get_inner_nodes_sets_of_c_let(Node);
get_inner_nodes_sets(_NodeType, _Node) ->
    [].

set_inner_nodes_sets(module, Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_module(Node, InnerNodesSets);
set_inner_nodes_sets('fun', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_fun(Node, InnerNodesSets);
set_inner_nodes_sets('let', Node, InnerNodesSets) ->
    set_inner_nodes_sets_of_c_let(Node, InnerNodesSets).

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

get_inner_nodes_sets_of_c_fun(Node) ->
    FunBody = cerl:fun_body(Node),
    InnerNodesSets = [[FunBody]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_fun(Node, [[FunBody]]) ->
    Node1 = cerl:update_c_fun(
              Node,
              cerl:fun_vars(Node),
              FunBody),
    Node1.

get_inner_nodes_sets_of_c_let(Node) ->
    Argument = cerl:let_arg(Node),
    Body = cerl:let_body(Node),
    InnerNodesSets = [[Argument], [Body]],
    InnerNodesSets.

set_inner_nodes_sets_of_c_let(Node, [[Argument], [Body]]) ->
    Node1 = cerl:update_c_let(
              Node,
              cerl:let_vars(Node),
              Argument,
              Body),
    Node1.
