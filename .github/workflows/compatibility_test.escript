#!/usr/bin/env escript
%%! -pa .github/workflows -pa _build/default/lib/horus/ebin

-define(OUTPUT_DIR, ".github/workflows").
-define(MOD_SOURCE, ?OUTPUT_DIR "/compatibility_test.erl").

main(["generate", Filename]) ->
    Mod = compile(),
    ok = Mod:generate(Filename),
    ok;
main(["run", Filename]) ->
    Mod = compile(),
    ok = Mod:run(Filename),
    ok.

compile() ->
    CompileOptions = [report,
                      return_errors,
                      return_warnings,
                      debug_info,
                      {outdir, ?OUTPUT_DIR}],
    {ok, Mod, []} = compile:file(?MOD_SOURCE, CompileOptions),
    Mod.
