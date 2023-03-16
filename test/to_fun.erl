%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(to_fun).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

standalone_fun_to_fun_test() ->
    Fun = fun() -> ok end,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    NewFun = horus:to_fun(StandaloneFun),
    ?assert(is_function(NewFun, 0)),
    ?assertEqual(Fun(), NewFun()).

fun_to_fun_test() ->
    Fun = fun erlang:abs/1,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assert(is_function(StandaloneFun, 1)),
    NewFun = horus:to_fun(StandaloneFun),
    ?assert(is_function(NewFun, 1)),
    ?assertEqual(Fun(-2), NewFun(-2)).
