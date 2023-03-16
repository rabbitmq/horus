%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(module_info).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

with_module_info_test() ->
    Fun = fun() -> ok end,
    StandaloneFun = horus:to_standalone_fun(
                      Fun, #{add_module_info => true}),
    ?assertStandaloneFun(StandaloneFun),
    ?assertMatch(ok, horus:exec(StandaloneFun, [])),

    #standalone_fun{module = Module} = StandaloneFun,
    ?assert(erlang:function_exported(Module, module_info, 0)),
    ?assert(erlang:function_exported(Module, module_info, 1)),
    ?assertEqual(erlang:get_module_info(Module), Module:module_info()).

without_module_info_test() ->
    Fun = fun() -> ok end,
    StandaloneFun = horus:to_standalone_fun(
                      Fun, #{add_module_info => false}),
    ?assertStandaloneFun(StandaloneFun),
    ?assertMatch(ok, horus:exec(StandaloneFun, [])),

    #standalone_fun{module = Module} = StandaloneFun,
    ?assertNot(erlang:function_exported(Module, module_info, 0)),
    ?assertNot(erlang:function_exported(Module, module_info, 1)).
