%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(fun_env).

-include_lib("eunit/include/eunit.hrl").

-include("src/horus_fun.hrl").

-include("test/helpers.hrl").

list_in_fun_env_test() ->
    List = make_list(),
    StandaloneFun = horus:to_standalone_fun(fun() -> List end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(List, horus:exec(StandaloneFun, [])).

make_list() -> [a, b].

map_in_fun_env_test() ->
    Map = make_map(),
    StandaloneFun = horus:to_standalone_fun(fun() -> Map end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(Map, horus:exec(StandaloneFun, [])).

make_map() -> #{a => b}.

tuple_in_fun_env_test() ->
    Tuple = make_tuple(),
    StandaloneFun = horus:to_standalone_fun(fun() -> Tuple end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(Tuple, horus:exec(StandaloneFun, [])).

make_tuple() -> {a, b}.

binary_in_fun_env_test() ->
    Binary = make_binary(),
    StandaloneFun = horus:to_standalone_fun(fun() -> Binary end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(Binary, horus:exec(StandaloneFun, [])).

make_binary() -> <<"ab">>.
