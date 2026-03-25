%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(compiling).

-include_lib("eunit/include/eunit.hrl").

-include("include/horus.hrl").
-include("src/horus_fun.hrl").
-include("test/helpers.hrl").

-dialyzer({nowarn_function, [can_compile_asm_test/0]}).

can_compile_asm_test() ->
    Mod = arbitrary_mod,
    CompileInfo = Mod:module_info(compile),
    SrcFile = proplists:get_value(source, CompileInfo),
    ModFile = code:which(Mod),

    CompilerOptions = ['S', {outdir, filename:dirname(ModFile)}],
    AsmFile = filename:rootname(ModFile) ++ ".S",
    {ok, Mod} = compile:file(SrcFile, CompilerOptions),

    try
        {ok, Asm} = file:consult(AsmFile),
        Asm1 = horus:file_asm_to_compiler_asm(Asm),
        Beam = horus:compile(Asm1),
        ?assertMatch(_ when is_binary(Beam), Beam),

        Fun = fun Mod:run/0,
        StandaloneFun0 = horus:to_standalone_fun(Fun),
        StandaloneFun1 = StandaloneFun0#horus_fun{module = Mod,
                                                  beam = Beam},

        _ = code:delete(Mod),
        _ = code:purge(Mod),
        ?assertNot(horus_utils:is_module_loaded(Mod)),
        ?assertNot(horus:is_standalone_fun_loaded(StandaloneFun1)),
        try
            ?assertMatch(yay, horus:exec(StandaloneFun1, []))
        after
             horus:unload_standalone_fun(StandaloneFun1)
        end
    after
        _ = file:delete(AsmFile)
    end,

    ?assertError(
       ?horus_exception(compilation_failure, #{}),
       horus:compile({})).
