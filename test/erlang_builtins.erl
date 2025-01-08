%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_builtins).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

node_bif_test() ->
    StandaloneFun = ?make_standalone_fun(node()),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(node(), horus:exec(StandaloneFun, [])).

self_bif_test() ->
    StandaloneFun = ?make_standalone_fun(self()),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(self(), horus:exec(StandaloneFun, [])).

apply_test() ->
    Module = helpers:ensure_not_optimized(erlang),
    StandaloneFun = ?make_standalone_fun(
                       begin
                           c = hd(Module:tl([[a, b], c])),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

%% `apply_last' instruction is used when the apply is the last call
%% in the function.
apply_last_test() ->
    Module = helpers:ensure_not_optimized(erlang),
    StandaloneFun = ?make_standalone_fun(
                       begin
                           Module:tl([[a, b], c])
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual([c], horus:exec(StandaloneFun, [])).
