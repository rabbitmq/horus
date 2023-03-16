%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(helpers).

-export([ensure_not_optimized/1]).

-spec ensure_not_optimized(Value) -> Value when
      Value :: term().
%% @doc Makes sure the given value is not optimized out by the compiler.
%%
%% The compiler is smart enough to optimize away many instructions by
%% inspecting types and values. This function confuses the compiler by sending
%% and receiving the value.

ensure_not_optimized(Value) ->
    self() ! Value,
    receive Msg -> Msg end.
