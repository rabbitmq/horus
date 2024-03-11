%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_lists).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

list_prepend_test() ->
    List = helpers:ensure_not_optimized([b]),
    StandaloneFun = ?make_standalone_fun([a | List]),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([a, b], horus:exec(StandaloneFun, [])).

list_concat_test() ->
    List = helpers:ensure_not_optimized([a]),
    StandaloneFun = ?make_standalone_fun(List ++ [b]),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([a, b], horus:exec(StandaloneFun, [])).

list_diff_test() ->
    List = helpers:ensure_not_optimized([a, b]),
    StandaloneFun = ?make_standalone_fun(List -- [b]),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([a], horus:exec(StandaloneFun, [])).

list_comprehension_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           [erlang:abs(I) || I <- [1, 2, 3]]
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([1, 2, 3], horus:exec(StandaloneFun, [])).

list_comprehension_with_conditions_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           [erlang:abs(I)
                            || I <- [1, 2, 3],
                               I >= 2]
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([2, 3], horus:exec(StandaloneFun, [])).

list_comprehension_with_multiple_qualifiers_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           [Value
                            || Props <- [[{a, 1}],
                                         [{b, 2}],
                                         [{c, 3}]],
                               {_, Value} <- Props]
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([1, 2, 3], horus:exec(StandaloneFun, [])).

list_pattern_matching_test() ->
    List = helpers:ensure_not_optimized([a, b, c]),

    %% Tests the get_list/3 instruction.
    Reverse = ?make_standalone_fun(reverse(List)),
    ?assertStandaloneFun(Reverse),
    ?assertEqual([c, b, a], horus:exec(Reverse, [])),

    %% Tests the get_hd/2 instruction.
    ReverseHead = ?make_standalone_fun(
                        begin
                            [Head | _] = reverse(List),
                            Head
                        end),
    ?assertStandaloneFun(ReverseHead),
    ?assertEqual(c, horus:exec(ReverseHead, [])).

reverse(List) ->
    reverse(List, []).

reverse([Head | Tail], Acc) ->
    reverse(Tail, [Head | Acc]);
reverse([], Acc) ->
    Acc.
