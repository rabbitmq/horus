%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(arbitrary_mod).

-export([run/0,
         min/2,
         call_inner_function/2]).

run() ->
    yay.

min(A, B) ->
    erlang:min(A, B).

call_inner_function(InnerFun, Options) when is_map(Options) ->
    InnerFun(Options).
