%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(using_erl_eval).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

-define(TX_CODE, "Fun = fun(Arg) -> erlang:abs(Arg) end,
                  horus:to_standalone_fun(Fun).").

erl_eval_test() ->
    Bindings = erl_eval:new_bindings(),
    {ok, Tokens, _EndLocation} = erl_scan:string(?TX_CODE),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, StandaloneFun, _NewBindings} = erl_eval:exprs(Exprs, Bindings),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(2, horus:exec(StandaloneFun, [-2])).
