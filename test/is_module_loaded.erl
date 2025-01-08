%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2023-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(is_module_loaded).

-include_lib("eunit/include/eunit.hrl").

on_a_loaded_module_test() ->
    _ = lists:module_info(),
    ?assert(horus_utils:is_module_loaded(lists)).

on_a_missing_module_test() ->
    Module = 'non_existing_module!',
    ?assertError(undef, Module:module_info()),
    ?assertNot(horus_utils:is_module_loaded(Module)).
