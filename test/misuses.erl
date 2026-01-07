%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(misuses).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").
-include("src/horus_fun.hrl").

-dialyzer([{no_missing_calls,
            [unknown_module_1_test/0,
             unknown_module_2_test/0,
             unknown_function_1_test/0,
             unknown_function_2_test/0,
             unexported_function_1_test/0,
             unexported_function_2_test/0]},
           {no_fail_call,
            [exec_wrong_arity_test/0]},
           {no_return,
            [exec_invalid_generated_module_test/0]}]).

unknown_module_1_test() ->
    Fun = fun not_a_module:not_a_function/0,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {not_a_module, not_a_function, 0}}),
       horus:to_standalone_fun(Fun)).

unknown_module_2_test() ->
    Fun = fun() -> not_a_module:not_a_function() end,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {not_a_module, not_a_function, 0}}),
       horus:to_standalone_fun(Fun)).

unknown_function_1_test() ->
    Fun = fun compile:not_a_function/0,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {compile, not_a_function, 0}}),
       horus:to_standalone_fun(Fun)).

unknown_function_2_test() ->
    Fun = fun() -> compile:not_a_function() end,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {compile, not_a_function, 0}}),
       horus:to_standalone_fun(Fun)).

unexported_function_1_test() ->
    Fun = fun compile:env_default_opts/0,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {compile, env_default_opts, 0}}),
       horus:to_standalone_fun(Fun)).

unexported_function_2_test() ->
    Fun = fun() -> compile:env_default_opts() end,
    ?assertError(undef, Fun()),
    ?assertThrow(
       ?horus_error(
          call_to_unexported_function,
          #{mfa := {compile, env_default_opts, 0}}),
       horus:to_standalone_fun(Fun)).

exec_wrong_arity_test() ->
    Fun = fun(Arg) -> Arg end,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertError({badarity, {Fun, []}}, Fun()),
    ?assertError(
       {badarity, {StandaloneFun, []}},
       horus:exec(StandaloneFun, [])).

exec_invalid_generated_module_test() ->
    StandaloneFun = #horus_fun{
                       module = module_not_loaded,
                       beam = <<"invalid">>,
                       arity = 0,
                       literal_funs = [],
                       fun_name_mapping = #{},
                       env = []},
    ?assertError(
       ?horus_exception(
          invalid_generated_module,
          #{horus_fun := StandaloneFun,
            error := {error, badfile}}),
       horus:exec(StandaloneFun, [])).
