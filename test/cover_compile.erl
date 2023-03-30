%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(cover_compile).

-include_lib("eunit/include/eunit.hrl").

-include("src/horus_fun.hrl").

cover_compilation_works_test() ->
    Module = cover_compiled_mod,
    Arg = arg,
    Ret = Module:run(Arg),

    Fun = fun Module:run/1,
    {_, _, ModFile} = code:get_object_code(Module),
    ?assertNot(cover:is_compiled(Module)),
    InitialState = [{{Module, run, 1},
                     {0, %% Covered
                      2  %% Not covered
                     }}],
    Analysis = [{{Module, run, 1},
                 {2, %% Covered
                  0  %% Not covered
                 }}],

    %% We extract the regular module before we cover-compile it. We do this to
    %% compare the standalone functions to make sure that their module names
    %% are different because the code and the checksum are different.
    StandaloneFun1 = horus:to_standalone_fun(Fun, #{debug_info => true}),
    ?assertEqual(Ret, horus:exec(StandaloneFun1, [Arg])),

    %% We ensure that cover-compilation works and that we get the expected
    %% analysis results.
    ?assertEqual({ok, Module}, cover:compile_beam(ModFile)),
    ?assertEqual({file, ModFile}, cover:is_compiled(Module)),
    ?assertEqual({ok, InitialState}, cover:analyse(Module)),

    ?assertEqual(Ret, Module:run(Arg)),
    ?assertEqual({ok, Analysis}, cover:analyse(Module)),

    ok = cover:reset(Module),

    %% Now, we try to extract the cover-compiled module. We then execute the
    %% standalone function and verify the analysis again.
    ?assertEqual({file, ModFile}, cover:is_compiled(Module)),
    ?assertEqual({ok, InitialState}, cover:analyse(Module)),

    StandaloneFun2 = horus:to_standalone_fun(Fun, #{debug_info => true}),
    ?assertEqual(Ret, horus:exec(StandaloneFun2, [Arg])),
    ?assertEqual({ok, Analysis}, cover:analyse(Module)),

    ok = cover:reset(Module),

    %% We finally compare the standalone functions: they must have a different
    %% module name and checksum.
    GeneratedName1 = StandaloneFun1#horus_fun.module,
    GeneratedName2 = StandaloneFun2#horus_fun.module,
    ?assertNotEqual(GeneratedName1, GeneratedName2),

    Info1 = maps:get(fun_info, StandaloneFun1#horus_fun.debug_info),
    Info2 = maps:get(fun_info, StandaloneFun2#horus_fun.debug_info),
    ?assertEqual(Info1, Info2),

    Checksums1 = maps:get(checksums, StandaloneFun1#horus_fun.debug_info),
    Checksums2 = maps:get(checksums, StandaloneFun2#horus_fun.debug_info),
    ?assertNotEqual(
       maps:get(Module, Checksums1),
       maps:get(Module, Checksums2)),

    ok.
