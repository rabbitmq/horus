%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_maps).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

match_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           Map = #{a => b},
                           #{a := Value} = Map,
                           Value
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(b, helpers:horus_exec(StandaloneFun, [])).

update_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           Map = #{a => b},
                           Map#{a => c}
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(#{a => c}, helpers:horus_exec(StandaloneFun, [])).
