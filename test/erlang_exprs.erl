%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_exprs).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

add_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three + 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(5, helpers:horus_exec(StandaloneFun, [])).

subtract_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three - 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1, helpers:horus_exec(StandaloneFun, [])).

multiply_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three * 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(6, helpers:horus_exec(StandaloneFun, [])).

divide_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three / 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1.5, helpers:horus_exec(StandaloneFun, [])).

integer_divide_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three div 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1, helpers:horus_exec(StandaloneFun, [])).

remainder_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three rem 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1, helpers:horus_exec(StandaloneFun, [])).

lt_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three < 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(false, helpers:horus_exec(StandaloneFun, [])).

le_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three =< 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(false, helpers:horus_exec(StandaloneFun, [])).

gt_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three > 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(true, helpers:horus_exec(StandaloneFun, [])).

ge_test() ->
    Three = helpers:ensure_not_optimized(3),
    StandaloneFun = ?make_standalone_fun(Three >= 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(true, helpers:horus_exec(StandaloneFun, [])).

binary_and_test() ->
    One = helpers:ensure_not_optimized(1),
    StandaloneFun = ?make_standalone_fun(One band 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(0, helpers:horus_exec(StandaloneFun, [])).

binary_or_test() ->
    One = helpers:ensure_not_optimized(1),
    StandaloneFun = ?make_standalone_fun(One bor 2),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(3, helpers:horus_exec(StandaloneFun, [])).

binary_lshift_test() ->
    One = helpers:ensure_not_optimized(1),
    StandaloneFun = ?make_standalone_fun(One bsl 1),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(2, helpers:horus_exec(StandaloneFun, [])).

binary_rshift_test() ->
    Two = helpers:ensure_not_optimized(2),
    StandaloneFun = ?make_standalone_fun(Two bsr 1),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1, helpers:horus_exec(StandaloneFun, [])).

logical_and_test() ->
    True = helpers:ensure_not_optimized(true),
    StandaloneFun = ?make_standalone_fun(True and false),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(false, helpers:horus_exec(StandaloneFun, [])).

logical_andalso_test() ->
    True = helpers:ensure_not_optimized(true),
    StandaloneFun = ?make_standalone_fun(True andalso false),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(false, helpers:horus_exec(StandaloneFun, [])).

logical_or_test() ->
    True = helpers:ensure_not_optimized(true),
    StandaloneFun = ?make_standalone_fun(True or false),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(true, helpers:horus_exec(StandaloneFun, [])).

logical_orelse_test() ->
    True = helpers:ensure_not_optimized(true),
    StandaloneFun = ?make_standalone_fun(True orelse false),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(true, helpers:horus_exec(StandaloneFun, [])).

logical_xor_test() ->
    True = helpers:ensure_not_optimized(true),
    StandaloneFun = ?make_standalone_fun(True xor false),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(true, helpers:horus_exec(StandaloneFun, [])).

send_message_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                            self() ! message,
                            receive Msg -> Msg end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(message, helpers:horus_exec(StandaloneFun, [])).
