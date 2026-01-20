%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(lazy_compilation).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").
-include("test/helpers.hrl").

immediate_compilation_works_test() ->
    Mod = arbitrary_mod,
    Fun = fun Mod:min/2,
    Args = [2, 3],
    Options = #{lazy_compilation => false},
    StandaloneFun = horus:to_standalone_fun(Fun, Options),

    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)),
    ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFun)),
    ?assertNot(horus:uses_lazy_compilation(StandaloneFun)),
    ?assertEqual(2, horus:exec(StandaloneFun, Args)),
    horus:unload_standalone_fun(StandaloneFun).

lazy_compilation_works_test() ->
    Mod = arbitrary_mod,
    Fun = fun Mod:min/2,
    Args = [2, 3],
    Options = #{lazy_compilation => true},
    StandaloneFun = horus:to_standalone_fun(Fun, Options),

    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)),
    ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFun)),
    ?assert(horus:uses_lazy_compilation(StandaloneFun)),
    ?assertEqual(2, horus:exec(StandaloneFun, Args)),
    horus:unload_standalone_fun(StandaloneFun).

uses_immediate_compilation_by_default_test() ->
    Mod = arbitrary_mod,
    Fun = fun Mod:min/2,
    Args = [2, 3],
    StandaloneFun = horus:to_standalone_fun(Fun),

    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)),
    ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFun)),
    ?assertNot(horus:uses_lazy_compilation(StandaloneFun)),
    ?assertEqual(2, horus:exec(StandaloneFun, Args)),
    horus:unload_standalone_fun(StandaloneFun).

lazy_compilation_does_not_affect_regular_funs_test() ->
    Fun = fun lists:min/1,
    List = [2, 3],
    StandaloneFun = horus:to_standalone_fun(Fun),

    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)),
    ?assertNot(?IS_HORUS_STANDALONE_FUN(StandaloneFun)),
    ?assertNot(horus:uses_lazy_compilation(StandaloneFun)),
    ?assertEqual(2, horus:exec(StandaloneFun, [List])),
    horus:unload_standalone_fun(StandaloneFun).
