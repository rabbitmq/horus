%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(local_vs_external).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

local_test() ->
    Fun = fun() -> ok end,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    ?assertMatch(ok, horus:exec(StandaloneFun, [])).

external_test() ->
    Fun = fun erlang:abs/1,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assert(is_function(StandaloneFun, 1)),
    ?assertEqual(Fun, StandaloneFun),
    ?assertMatch(1, horus:exec(StandaloneFun, [1])).
