%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(is_standalone_fun_loaded).

-include_lib("eunit/include/eunit.hrl").

can_unload_standalone_function_test() ->
    Fun = fun() -> ok end,

    ?assertNot(horus:is_standalone_fun_loaded(Fun)),
    ?assertEqual(ok, horus:unload_standalone_fun(Fun)),

    StandaloneFun = horus:to_standalone_fun(Fun),

    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)),

    ?assertEqual(ok, horus:exec(StandaloneFun, [])),
    ?assert(horus:is_standalone_fun_loaded(StandaloneFun)),

    ?assertEqual(ok, horus:unload_standalone_fun(StandaloneFun)),
    ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun)).
