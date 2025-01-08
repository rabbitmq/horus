%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2023-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(macros).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").
-include("test/helpers.hrl").

is_horus_fun_test() ->
    Fun = fun() -> ok end,
    ?assert(?IS_HORUS_FUN(Fun)),

    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    ?assert(?IS_HORUS_FUN(StandaloneFun)),

    ?assertNot(?IS_HORUS_FUN(not_a_horus_fun)).

is_standalone_horus_fun_test() ->
    Fun = fun() -> ok end,
    ?assertNot(?IS_HORUS_STANDALONE_FUN(Fun)),

    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFun)),

    ?assertNot(?IS_HORUS_STANDALONE_FUN(not_a_horus_fun)).

is_standalone_horus_fun_arity_test() ->
    Fun = fun() -> ok end,
    ?assertNot(?IS_HORUS_STANDALONE_FUN(Fun, 0)),

    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFun, 0)),
    ?assertNot(?IS_HORUS_STANDALONE_FUN(StandaloneFun, 1)),

    ?assertNot(?IS_HORUS_STANDALONE_FUN(not_a_horus_fun, 0)).

standalone_horus_arity_test() ->
    Fun = fun() -> ok end,
    ?assertError(
       badarg,
       ?HORUS_STANDALONE_FUN_ARITY(helpers:ensure_not_optimized(Fun))),

    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(0, ?HORUS_STANDALONE_FUN_ARITY(StandaloneFun)),

    ?assertError(
       badarg,
       ?HORUS_STANDALONE_FUN_ARITY(
          helpers:ensure_not_optimized(not_a_horus_fun))).
