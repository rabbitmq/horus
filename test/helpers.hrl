%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-include_lib("eunit/include/eunit.hrl").

-include("src/horus_fun.hrl").

-define(make_standalone_fun(Expression),
        fun() ->
            __Fun = fun() -> Expression end,
            horus:to_standalone_fun(__Fun, #{debug_info => true})
        end()).

-define(assertStandaloneFun(StandaloneFun),
        ?assertMatch(#horus_fun{}, StandaloneFun)).

-define(assertToFunError(Expected, Expression),
        ?assertError(Expected, ?make_standalone_fun(Expression))).
