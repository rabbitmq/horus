%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_records).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

-record(my_record, {field}).
-record(pair, {a, b}).

create_record_test() ->
    Field = helpers:ensure_not_optimized(ok),
    StandaloneFun = ?make_standalone_fun(#my_record{field = Field}),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(#my_record{field = Field}, horus:exec(StandaloneFun, [])).

update_record_test() ->
    Record = helpers:ensure_not_optimized(#pair{a = 123, b = 456}),
    StandaloneFun = ?make_standalone_fun(Record#pair{a = 789}),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(#pair{a = 789, b = 456}, horus:exec(StandaloneFun, [])).

match_record_test() ->
    Record = helpers:ensure_not_optimized(#my_record{field = ok}),
    StandaloneFun = ?make_standalone_fun(
                       begin
                           #my_record{field = Field} = Record,
                           Field
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).
