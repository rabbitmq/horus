%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_blocks).

-if(?OTP_RELEASE >= 25).
-feature(maybe_expr,enable).
-endif.

-if(?OTP_RELEASE >= 29).
-compile(nowarn_deprecated_catch).
-endif.

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

begin_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

case_test() ->
    StandaloneFun = ?make_standalone_fun(
                       case helpers:ensure_not_optimized({a, b}) of
                           {_}       -> error;
                           {_, _}    -> ok;
                           {_, _, _} -> error
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

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
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

receive_any_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           self() ! ?FUNCTION_NAME,
                           receive
                               Msg -> Msg
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(?FUNCTION_NAME, helpers:horus_exec(StandaloneFun, [])).

receive_match_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           self() ! ?FUNCTION_NAME,
                           receive
                               ?FUNCTION_NAME -> ok;
                               _              -> error
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

receive_after_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           self() ! ?FUNCTION_NAME,
                           receive
                               Msg -> Msg
                           after 10000 -> error
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(?FUNCTION_NAME, helpers:horus_exec(StandaloneFun, [])).

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
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

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
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

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
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

catch_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           _ = catch (0 = helpers:ensure_not_optimized(1)),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, helpers:horus_exec(StandaloneFun, [])).

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
    ?assertError({badmatch, 1}, helpers:horus_exec(StandaloneFun, [])).

-if(?OTP_RELEASE >= 25).
-if(?FEATURE_ENABLED(maybe_expr)).
maybe_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           maybe
                               {ok, A} ?= erlang:list_to_tuple([ok, 42]),
                               true = A >= 0,
                               {ok, B} ?= erlang:list_to_tuple([ok, 58]),
                               A + B
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(100, helpers:horus_exec(StandaloneFun, [])).

maybe_else_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           maybe
                               {ok, A} ?= erlang:list_to_tuple([ok, 42]),
                               true = A >= 0,
                               {ok, B} ?= receive
                                              {not_receiving, Msg} -> Msg
                                          after 0 ->
                                                    {error, reason}
                                          end,
                               A + B
                           else
                               {error, Reason} ->
                                   Reason
                           end
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(reason, helpers:horus_exec(StandaloneFun, [])).
-endif.
-endif.
