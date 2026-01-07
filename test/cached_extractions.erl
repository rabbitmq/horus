%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(cached_extractions).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

-dialyzer({nowarn_function, [my_fun_module/1,
                             modified_module_causes_cache_miss_test/0]}).

local_fun_test() ->
    Fun = fun() -> ok end,
    StandaloneFun1 = horus:to_standalone_fun(Fun),
    StandaloneFun2 = horus:to_standalone_fun(Fun),
    ?assertEqual(StandaloneFun1, StandaloneFun2),
    ?assertEqual(ok, horus:exec(StandaloneFun1, [])),
    ?assertEqual(ok, horus:exec(StandaloneFun2, [])).

external_fun_test() ->
    Fun = fun erlang:abs/1,
    StandaloneFun1 = horus:to_standalone_fun(Fun),
    StandaloneFun2 = horus:to_standalone_fun(Fun),
    ?assertEqual(StandaloneFun1, StandaloneFun2),
    ?assertEqual(1, horus:exec(StandaloneFun1, [1])),
    ?assertEqual(1, horus:exec(StandaloneFun2, [1])).

standalone_fun_is_cached_test() ->
    Fun = fun() -> ok end,

    #{module := Module,
      name := Name,
      arity := Arity,
      type := local,
      new_uniq := Checksum} = maps:from_list(erlang:fun_info(Fun)),
    Options = #{},
    Key = horus:standalone_fun_cache_key(
            Module, Name, Arity, Checksum, Options),

    StandaloneFun1 = horus:to_standalone_fun(Fun, Options),
    CacheEntry1 = persistent_term:get(Key, undefined),
    ?assertStandaloneFun(StandaloneFun1),
    ?assertMatch(#{horus_fun := StandaloneFun1}, CacheEntry1),
    #{counters := Counters} = CacheEntry1,
    ?assertEqual(0, counters:get(Counters, 1)),

    StandaloneFun2 = horus:to_standalone_fun(Fun, Options),
    CacheEntry2 = persistent_term:get(Key, undefined),
    ?assertEqual(StandaloneFun1, StandaloneFun2),
    ?assertEqual(CacheEntry1, CacheEntry2),
    ?assertEqual(1, counters:get(Counters, 1)),

    StandaloneFun3 = horus:to_standalone_fun(Fun, Options),
    CacheEntry3 = persistent_term:get(Key, undefined),
    ?assertEqual(StandaloneFun1, StandaloneFun3),
    ?assertEqual(CacheEntry1, CacheEntry3),
    ?assertEqual(2, counters:get(Counters, 1)).

kept_fun_is_cached_test() ->
    Fun = fun() -> ok end,

    #{module := Module,
      name := Name,
      arity := Arity,
      type := local,
      new_uniq := Checksum} = maps:from_list(erlang:fun_info(Fun)),
    Options = #{should_process_function =>
                fun(_Module, _Function, _Arity, _FromModule) -> false end},
    Key = horus:standalone_fun_cache_key(
            Module, Name, Arity, Checksum, Options),

    StandaloneFun1 = horus:to_standalone_fun(Fun, Options),
    CacheEntry1 = persistent_term:get(Key, undefined),
    ?assertEqual(Fun, StandaloneFun1),
    ?assertMatch(#{fun_kept := true}, CacheEntry1),
    #{counters := Counters} = CacheEntry1,
    ?assertEqual(0, counters:get(Counters, 1)),

    StandaloneFun2 = horus:to_standalone_fun(Fun, Options),
    CacheEntry2 = persistent_term:get(Key, undefined),
    ?assertEqual(StandaloneFun1, StandaloneFun2),
    ?assertEqual(CacheEntry1, CacheEntry2),
    ?assertEqual(1, counters:get(Counters, 1)),

    StandaloneFun3 = horus:to_standalone_fun(Fun, Options),
    CacheEntry3 = persistent_term:get(Key, undefined),
    ?assertEqual(StandaloneFun1, StandaloneFun3),
    ?assertEqual(CacheEntry1, CacheEntry3),
    ?assertEqual(2, counters:get(Counters, 1)).

different_options_means_different_cache_entries_test() ->
    Fun = fun() -> ok end,

    Options1 = #{},
    Options2 = #{should_process_function =>
                 fun(_Module, _Function, _Arity, _FromModule) -> false end},

    StandaloneFun1 = horus:to_standalone_fun(Fun, Options1),
    StandaloneFun2 = horus:to_standalone_fun(Fun, Options2),
    ?assertStandaloneFun(StandaloneFun1),
    ?assertEqual(Fun, StandaloneFun2).

my_fun_module(Version) ->
    Module = my_fun,
    Asm = {Module, %% Module
           [{module_info,0}, %% Exports
            {module_info,1},
            {version,0}],
           [], %% Attributes
           [
            {function, version, 0, 2,
             [
              {label, 1},
              {func_info, {atom, Module}, {atom, version},0},
              {label, 2},
              {move, {integer, Version}, {x, 0}},
              return
             ]},
            {function, module_info, 0, 4,
             [
              {label, 3},
              {func_info, {atom, Module}, {atom, module_info}, 0},
              {label, 4},
              {move, {atom, Module}, {x, 0}},
              {call_ext_only, 1, {extfunc, erlang, get_module_info, 1}}
             ]},
            {function, module_info, 1, 6,
             [
              {label, 5},
              {func_info, {atom, Module}, {atom, module_info}, 1},
              {label, 6},
              {move, {x, 0}, {x, 1}},
              {move, {atom, Module}, {x, 0}},
              {call_ext_only, 2, {extfunc, erlang, get_module_info, 2}}
             ]}
           ], %% Functions
           7 %% Label
          },
    horus:compile(Asm).

modified_module_causes_cache_miss_test() ->
    {Module, Beam1} = my_fun_module(1),
    {Module, Beam2} = my_fun_module(2),

    Options = #{},

    horus:override_object_code(Module, Beam1),
    ?assertEqual(
       {Module, Beam1, "", code_server},
       horus:get_object_code(Module)),
    ?assertEqual({module, Module}, code:load_binary(Module, "", Beam1)),
    ?assert(erlang:function_exported(Module, version, 0)),
    Fun1 = fun Module:version/0,
    #{module := Module,
      name := Name1,
      arity := Arity1,
      type := external} = maps:from_list(erlang:fun_info(Fun1)),
    Checksum1 = Module:module_info(md5),
    Key1 = horus:standalone_fun_cache_key(
             Module, Name1, Arity1, Checksum1, Options),

    StandaloneFun1 = horus:to_standalone_fun(Fun1, Options),
    CacheEntry1 = persistent_term:get(Key1, undefined),
    ?assertStandaloneFun(StandaloneFun1),
    ?assertEqual(1, horus:exec(StandaloneFun1, [])),
    #{counters := Counters1} = CacheEntry1,
    ?assertEqual(0, counters:get(Counters1, 1)),

    true = code:delete(Module),
    _ = code:purge(Module),

    horus:override_object_code(Module, Beam2),
    ?assertEqual(
       {Module, Beam2, "", code_server},
       horus:get_object_code(Module)),
    ?assertEqual({module, Module}, code:load_binary(Module, "", Beam2)),
    ?assert(erlang:function_exported(Module, version, 0)),
    Fun2 = fun Module:version/0,
    #{module := Module,
      name := Name2,
      arity := Arity2,
      type := external} = maps:from_list(erlang:fun_info(Fun2)),
    Checksum2 = Module:module_info(md5),
    Key2 = horus:standalone_fun_cache_key(
             Module, Name2, Arity2, Checksum2, Options),
    ?assertEqual(Name1, Name2),
    ?assertEqual(Arity1, Arity2),
    ?assertNotEqual(Checksum1, Checksum2),

    StandaloneFun2 = horus:to_standalone_fun(Fun2, Options),
    CacheEntry2 = persistent_term:get(Key2, undefined),
    ?assertStandaloneFun(StandaloneFun2),
    ?assertEqual(2, horus:exec(StandaloneFun2, [])),
    #{counters := Counters2} = CacheEntry2,
    ?assertEqual(0, counters:get(Counters2, 1)),

    true = code:delete(Module),
    _ = code:purge(Module).

callback_options_impact_cache_key_test() ->
    Fun = fun() -> ok end,
    Options1 = #{should_process_function =>
                 fun should_process_function_1/4},
    Options2 = #{should_process_function =>
                 fun should_process_function_2/4},
    StandaloneFun1 = horus:to_standalone_fun(Fun, Options1),
    StandaloneFun2 = horus:to_standalone_fun(Fun, Options2),
    ?assertStandaloneFun(StandaloneFun1),
    ?assertStandaloneFun(StandaloneFun2),
    ?assertEqual(StandaloneFun1, StandaloneFun2),

    #{module := Module,
      name := Name,
      arity := Arity,
      type := local} = maps:from_list(erlang:fun_info(Fun)),
    Checksum = Module:module_info(md5),
    Key1 = horus:standalone_fun_cache_key(
             Module, Name, Arity, Checksum, Options1),
    Key2 = horus:standalone_fun_cache_key(
             Module, Name, Arity, Checksum, Options2),
    ?assertNotEqual(Key1, Key2),

    CacheEntry1 = persistent_term:get(Key1, undefined),
    ?assertMatch(#{horus_fun := StandaloneFun1}, CacheEntry1),
    #{counters := Counters1} = CacheEntry1,
    ?assertEqual(0, counters:get(Counters1, 1)),

    CacheEntry2 = persistent_term:get(Key2, undefined),
    ?assertMatch(#{horus_fun := StandaloneFun2}, CacheEntry2),
    #{counters := Counters2} = CacheEntry2,
    ?assertEqual(0, counters:get(Counters2, 1)).

should_process_function_1(_Module, _Function, _Arity, _FromModule) ->
    true.

should_process_function_2(_Module, _Function, _Arity, _FromModule) ->
    true.
