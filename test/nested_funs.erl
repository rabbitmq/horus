%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(nested_funs).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").

-include("test/helpers.hrl").

-dialyzer([{no_return,
            [call_fun2_instruction_with_atom_unsafe_test/0,
             call_outer_function_external/3]},
           {no_fail_call,
            [call_outer_function_external/3]}]).

fun0_in_fun_env_test() ->
    Fun = make_fun(0),
    StandaloneFun = ?make_standalone_fun(Fun()),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun1_in_fun_env_test() ->
    Fun = make_fun(1),
    StandaloneFun = ?make_standalone_fun(Fun(1)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun2_in_fun_env_test() ->
    Fun = make_fun(2),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun3_in_fun_env_test() ->
    Fun = make_fun(3),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun4_in_fun_env_test() ->
    Fun = make_fun(4),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun5_in_fun_env_test() ->
    Fun = make_fun(5),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun6_in_fun_env_test() ->
    Fun = make_fun(6),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5, 6)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun7_in_fun_env_test() ->
    Fun = make_fun(7),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5, 6, 7)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun8_in_fun_env_test() ->
    Fun = make_fun(8),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5, 6, 7, 8)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun9_in_fun_env_test() ->
    Fun = make_fun(9),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun10_in_fun_env_test() ->
    Fun = make_fun(10),
    StandaloneFun = ?make_standalone_fun(Fun(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertEqual(result, horus:exec(StandaloneFun, [])).

fun11_in_fun_env_test() ->
    Fun = make_fun(11),
    %% TODO: Raise an error at extraction time, not at execution time.
    StandaloneFun = ?make_standalone_fun(
                       Fun(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertNotEqual([], StandaloneFun#horus_fun.env),
    ?assertError(
       ?horus_exception(
          nested_fun_with_arity_too_great,
          #{arity := 11}),
       horus:exec(StandaloneFun, [])).

make_fun(0)  -> fun() -> result end;
make_fun(1)  -> fun(_) -> result end;
make_fun(2)  -> fun(_, _) -> result end;
make_fun(3)  -> fun(_, _, _) -> result end;
make_fun(4)  -> fun(_, _, _, _) -> result end;
make_fun(5)  -> fun(_, _, _, _, _) -> result end;
make_fun(6)  -> fun(_, _, _, _, _, _) -> result end;
make_fun(7)  -> fun(_, _, _, _, _, _, _) -> result end;
make_fun(8)  -> fun(_, _, _, _, _, _, _, _) -> result end;
make_fun(9)  -> fun(_, _, _, _, _, _, _, _, _) -> result end;
make_fun(10) -> fun(_, _, _, _, _, _, _, _, _, _) -> result end;
make_fun(11) -> fun(_, _, _, _, _, _, _, _, _, _, _) -> result end.

higher_order_external_call_test() ->
    StandaloneFun = ?make_standalone_fun(
                        begin
                            Fun = fun arbitrary_mod:min/2,
                            apply_fun_to_args(Fun, 1, 2)
                        end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(1, horus:exec(StandaloneFun, [])).

apply_fun_to_args(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

multiple_nested_higher_order_functions_test() ->
    StandaloneFun = ?make_standalone_fun(
                        begin
                            MapFun = fun(X, Y) -> {X, Y} end,
                            Fun = fun projection_fun_for_sets/1,
                            Fun(MapFun)
                        end),
    ?assertStandaloneFun(StandaloneFun),
    Ret1 = horus:exec(StandaloneFun, []),
    ?assert(is_function(Ret1, 3)),

    Sets = sets:from_list([a, b]),
    Ret2 = Ret1(Sets, path, change),
    ?assertEqual([{change, {path, a}}, {change, {path, b}}], Ret2).

projection_fun_for_sets(MapFun) ->
    ChangesFromSet =
      fun(Set, Path, Change) ->
              sets:fold(fun(Element, Acc) ->
                                [{Change, MapFun(Path, Element)} | Acc]
                        end, [], Set)
      end,
    fun(Set, Path, Change) ->
            ChangesFromSet(Set, Path, Change)
    end.

call_fun2_instruction_with_atom_unsafe_test() ->
    OuterFun = fun(Ret) -> {outer, Ret} end,
    InnerFun = fun inner_function/1,

    StandaloneFun = ?make_standalone_fun(
                        begin
                            R = call_outer_function_external(
                                  OuterFun, InnerFun, #{}),
                            {ok, R}
                        end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertError({badarity, {_, [#{}]}}, horus:exec(StandaloneFun, [])).

-spec call_outer_function_external(fun(), fun(), map()) -> no_return().

call_outer_function_external(OuterFun, InnerFun, Options) ->
    OuterFun(
      arbitrary_mod:call_inner_function(
        fun() -> InnerFun() end,
        Options)).

call_fun2_instruction_with_jump_label_test() ->
    OuterFun = fun(Ret) -> {outer, Ret} end,
    InnerInnerFun = fun inner_function/1,
    InnerFun = fun() ->
                       case erlang:phash2(a) of
                           H when H > 1 -> InnerInnerFun(H);
                           _            -> error
                       end
               end,

    StandaloneFun = ?make_standalone_fun(
                        begin
                            R = call_outer_function_local(
                                  OuterFun, InnerFun, #{}),
                            {ok, R}
                        end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual({ok, {outer, inner}}, horus:exec(StandaloneFun, [])).

call_outer_function_local(OuterFun, InnerFun, Options) ->
    OuterFun(
      call_inner_function(
        fun() -> InnerFun() end,
        Options)).

call_inner_function(InnerFun, Options) when is_map(Options) ->
    InnerFun().

inner_function(_) ->
    inner.
