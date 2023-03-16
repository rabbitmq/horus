%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(erlang_blocks).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

begin_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

case_test() ->
    StandaloneFun = ?make_standalone_fun(
                       case helpers:ensure_not_optimized({a, b}) of
                           {_}       -> error;
                           {_, _}    -> ok;
                           {_, _, _} -> error
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

if_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           Ret = helpers:ensure_not_optimized(a),
                           if
                               Ret =:= a -> ok;
                               true      -> error
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

receive_any_test() ->
    self() ! ?FUNCTION_NAME,
    StandaloneFun = ?make_standalone_fun(
                       begin
                           receive
                               Msg -> Msg
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(?FUNCTION_NAME, horus:exec(StandaloneFun, [])).

receive_match_test() ->
    self() ! ?FUNCTION_NAME,
    StandaloneFun = ?make_standalone_fun(
                       begin
                           receive
                               ?FUNCTION_NAME -> ok;
                               _              -> error
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

receive_after_test() ->
    self() ! ?FUNCTION_NAME,
    StandaloneFun = ?make_standalone_fun(
                       begin
                           receive
                               Msg -> Msg
                           after 10000 -> error
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(?FUNCTION_NAME, horus:exec(StandaloneFun, [])).

try_catch_test() ->
    StandaloneFun = ?make_standalone_fun(
                       try
                           0 = helpers:ensure_not_optimized(1),
                           error
                       catch
                           _:_ ->
                               ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

try_catch_after_test() ->
    StandaloneFun = ?make_standalone_fun(
                       try
                           0 = helpers:ensure_not_optimized(1),
                           error
                       catch
                           _:_ ->
                               ok
                       after
                                 nothing_returned
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

try_of_test() ->
    StandaloneFun = ?make_standalone_fun(
                       try helpers:ensure_not_optimized(1) of
                           0 -> error;
                           1 -> ok;
                           2 -> error
                       catch
                           _:_ ->
                               ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

catch_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           _ = catch (0 = helpers:ensure_not_optimized(1)),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

raise_test() ->
    StandaloneFun = ?make_standalone_fun(
                       try
                           0 = helpers:ensure_not_optimized(1),
                           error
                       catch
                           Class:Reason:Stacktrace ->
                               erlang:raise(Class, Reason, Stacktrace)
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertError({badmatch, 1}, horus:exec(StandaloneFun, [])).
