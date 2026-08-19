%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_literals).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

atom_test() ->
    StandaloneFun = ?make_standalone_fun(ok),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

small_int_test() ->
    StandaloneFun = ?make_standalone_fun(3),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(3, helpers:horus_exec(StandaloneFun, [])).

large_int_test() ->
    StandaloneFun = ?make_standalone_fun(576460752303423489),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(576460752303423489, helpers:horus_exec(StandaloneFun, [])).

float_test() ->
    StandaloneFun = ?make_standalone_fun(3.0),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(3.0, helpers:horus_exec(StandaloneFun, [])).

binary_test() ->
    StandaloneFun = ?make_standalone_fun(<<"3">>),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(<<"3">>, helpers:horus_exec(StandaloneFun, [])).

list_test() ->
    StandaloneFun = ?make_standalone_fun([a, b]),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([a, b], helpers:horus_exec(StandaloneFun, [])).

map_test() ->
    StandaloneFun = ?make_standalone_fun(#{a => b}),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(#{a => b}, helpers:horus_exec(StandaloneFun, [])).
