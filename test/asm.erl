%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(asm).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").
-include("test/helpers.hrl").

supports_extraction_from_asm_test() ->
    Mod = arbitrary_mod,
    Fun = fun Mod:min/2,
    Args = [2, 3],

    %% We first extract the function using the usual `beam_disasm' to compare
    %% the result.
    StandaloneFunFromDisasm = horus:to_standalone_fun(Fun),

    %% We need to clear the cache. In the process, we ensure the standalone
    %% function input was from `beam_disasm'.
    lists:foreach(
      fun
          ({{horus, asm_cache, Module, _} = Key, {_, Props}})
            when Module =:= Mod ->
              ?assertEqual(beam_disasm, maps:get(asm_from, Props)),
              persistent_term:erase(Key);
          (_) ->
              ok
      end, persistent_term:get()),

    %% We re-compile the module but ask for the assembly instead. The assembly
    %% is written to an `.S' file next to the `.beam' one.
    CompileInfo = Mod:module_info(compile),
    SrcFile = proplists:get_value(source, CompileInfo),
    ModFile = code:which(Mod),
    AsmFile = filename:rootname(ModFile) ++ ".S",
    CompilerOptions = ['S',
                       {outdir, filename:dirname(ModFile)}],
    {ok, Mod} = compile:file(SrcFile, CompilerOptions),

    try
        %% We extract the function again. This time, Horus should pick up the
        %% `.S' file automatically. We verify that while clearing the cache
        %% again.
        StandaloneFunFromAsm = horus:to_standalone_fun(Fun),
        ?assert(?IS_HORUS_STANDALONE_FUN(StandaloneFunFromAsm)),
        lists:foreach(
          fun
              ({{horus, asm_cache, Module, _} = Key, {_, Props}})
                when Module =:= Mod ->
              ?assertEqual(AsmFile, maps:get(asm_from, Props)),
              persistent_term:erase(Key);
          (_) ->
              ok
          end, persistent_term:get()),

        %% Now, we compare the execution of both standalone functions.
        ?assertNot(horus:is_standalone_fun_loaded(StandaloneFunFromDisasm)),
        ?assertEqual(2, horus:exec(StandaloneFunFromDisasm, Args)),
        horus:unload_standalone_fun(StandaloneFunFromDisasm),

        ?assertNot(horus:is_standalone_fun_loaded(StandaloneFunFromAsm)),
        ?assertEqual(2, horus:exec(StandaloneFunFromAsm, Args)),
        horus:unload_standalone_fun(StandaloneFunFromAsm),
        ok
    after
        ok = file:delete(AsmFile),
        ok
    end.
