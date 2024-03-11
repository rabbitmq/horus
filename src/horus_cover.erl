%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2023-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

%% @private

-module(horus_cover).

-include_lib("stdlib/include/assert.hrl").

-include("src/horus_error.hrl").
-include("src/horus_fun.hrl").

-export([isolate_cover_instructions/2,
         determine_cover_patterns_locally/0]).

%% -------------------------------------------------------------------
%% Helpers to detect cover-specific instructions.
%% -------------------------------------------------------------------

-define(COVER_HELPER_MOD, horus_cover_helper).

-spec isolate_cover_instructions(MFA, Instructions) -> NewInstructions when
      MFA :: {Module, Name, Arity},
      Module :: module(),
      Name :: atom(),
      Arity :: arity(),
      Instructions :: [horus:beam_instr()],
      NewInstructions :: [horus:beam_instr() |
                          {'$cover', [horus:beam_instr()]}].
%% @doc Finds and isolates cover-specific instructions.
%%
%% If this function finds cover-specific instructions, it will isolate them in
%% a ``{'$cover', [...]}'' tuple. The caller is responsible for processing
%% those isolated instructions however it wants then put them back in the
%% regular flow of instructions.
%%
%% The module named `?COVER_HELPER_MOD' is explicitly ignored by this
%% function to avoid an infinite loop in the method used to determine
%% cover-specific instructions.
%%
%% @private

isolate_cover_instructions({?COVER_HELPER_MOD, _, _}, Instructions) ->
    %% The `?COVER_HELPER_MOD' module is used to determine the cover-specific
    %% instructions. As part of this, the code goes through this function too.
    %% To avoid an infinite recursion, we special-case this module and don't
    %% call {@link get_cover_instructions_patterns/0}.
    Instructions;
isolate_cover_instructions(_MFA, Instructions) ->
    %% We first get the cached cover instructions patterns (or determine these
    %% patterns if this is the first time), then we inspect the given
    %% instructions to see if we find cover-specific instructions.
    CoverPatterns = get_cover_instructions_patterns(),
    isolate_cover_instructions(Instructions, CoverPatterns, []).

isolate_cover_instructions(Instructions, CoverPatterns, Result)
  when length(Instructions) < length(CoverPatterns) ->
    %% We are too close to the end of the function body, there are not enough
    %% instructions left to ever match the cover-specific instructions
    %% patterns.
    %%
    %% We can return what we have processed so far and append the remaining
    %% instructions.
    lists:reverse(Result) ++ Instructions;
isolate_cover_instructions(Instructions, CoverPatterns, Result) ->
    %% We evaluate the patterns against the head of `Instructions'.
    case isolate_cover_instructions1(Instructions, CoverPatterns, []) of
        {IsolatedCoverInstructions, Rest} ->
            %% We found cover-specific instructions we can isolate. We then
            %% continue with the rest of the function body.
            Result1 = [{'$cover', IsolatedCoverInstructions} | Result],
            isolate_cover_instructions(Rest, CoverPatterns, Result1);
        false ->
            %% The head of `Instructions' didn't match the patterns. Let's
            %% skip one instruction and try again.
            [Instruction | Rest] = Instructions,
            Result1 = [Instruction | Result],
            isolate_cover_instructions(Rest, CoverPatterns, Result1)
    end.

isolate_cover_instructions1(
  [Instr | TailInstrs],
  [Pattern | TailPatterns] = Patterns,
  Result) ->
    %% The first instruction must match the first pattern before we consider
    %% following patterns.
    %%
    %% When a pattern matches, we move on to the next pattern until all of
    %% them matched an instruction. If we reach the last instructions but not
    %% all patterns matched, it means the instructions starting at `Instr' are
    %% not a series of cover-specific instructions.
    %%
    %% There may be other unimportant instructions in between cover-specific
    %% instructions, such as `{line, _}' annotations. Except for the first
    %% instruction, that's why if an instruction doesn't match a pattern, we
    %% put it in the result and move on to the next instruction until a
    %% pattern matches.
    case ets:match_spec_run([Instr], Pattern) of
        [match] ->
            %% A pattern matched, we continue with the next pattern.
            Result1 = [Instr | Result],
            isolate_cover_instructions1(TailInstrs, TailPatterns, Result1);
        _ when Result =:= [] ->
            %% The first pattern must match the very first instruction. Here,
            %% it didn't, thus `Instructions' are not a series of
            %% cover-specific instructions.
            false;
        _ ->
            %% The instruction didn't match, but that doesn't rule out a
            %% cover-specific series of instructions because there could be
            %% unimportant instructions in between.
            Result1 = [Instr | Result],
            isolate_cover_instructions1(TailInstrs, Patterns, Result1)
    end;
isolate_cover_instructions1(Instructions, [], Result) ->
    %% All patterns matched, we have a winner!
    {lists:reverse(Result), Instructions};
isolate_cover_instructions1([], Patterns, _Result) when Patterns =/= [] ->
    %% We reach the end of `Instructions' and some pattern were not matched.
    %% This is not a cover-specific series of instructions.
    false.

-define(COVER_INSTRUCTIONS_CACHE_KEY, {horus, asm_cache, cover_instructions}).

-spec get_cover_instructions_patterns() -> CoverPatterns when
      CoverPatterns :: [ets:compiled_match_spec()].
%% @doc Returns the cover-specific instructions patterns.
%%
%% The patterns are cached. If the cache is empty, we acquire a lock and run
%% the function to determine the patterns. The result is cached and the lock
%% is released.
%%
%% @private

get_cover_instructions_patterns() ->
    case persistent_term:get(?COVER_INSTRUCTIONS_CACHE_KEY, undefined) of
        CoverPatterns when CoverPatterns =/= undefined ->
            CoverPatterns;
        undefined ->
            Lock = {?COVER_INSTRUCTIONS_CACHE_KEY, self()},
            global:set_lock(Lock, [node()]),
            try
                get_cover_instructions_patterns_locked()
            after
                global:del_lock(Lock)
            end
    end.

-spec get_cover_instructions_patterns_locked() -> CoverPatterns when
      CoverPatterns :: [ets:compiled_match_spec()].
%% @doc Returns the cover-specific instructions patterns.
%%
%% It looks at the cache first, then call {@link determine_cover_patterns/0}
%% if the cache is empty.
%%
%% This function must be called with the lock held. Otherwise concurrent calls
%% will fail to determine the cover-specific instructions as they will
%% concurrently cover-compile and load the same module.
%%
%% @private

get_cover_instructions_patterns_locked() ->
    case persistent_term:get(?COVER_INSTRUCTIONS_CACHE_KEY, undefined) of
        undefined ->
            CoverPatterns = determine_cover_patterns(),
            persistent_term:put(?COVER_INSTRUCTIONS_CACHE_KEY, CoverPatterns),
            CoverPatterns;
        CoverPatterns ->
            CoverPatterns
    end.

-spec determine_cover_patterns() -> CoverPatterns when
      CoverPatterns :: [ets:compiled_match_spec()].
%% @doc Determines the cover-specific instructions patterns, regardless of the
%% executing node.
%%
%% @private.

determine_cover_patterns() ->
    %% Cover-compilation only works on the main node, thus we may need to
    %% perform an RPC call.
    CoverPatterns = case cover:get_main_node() of
                        Node when Node =:= node() ->
                            determine_cover_patterns_locally();
                        Node ->
                            determine_cover_patterns_remotely(Node)
                    end,
    %% However, the compilation of an ETS match spec is only valid locally as
    %% it returns a reference.
    CoverMatchSpecs = [begin
                           MatchSpec = {Pattern, [], [match]},
                           ets:match_spec_compile([MatchSpec])
                       end || Pattern <- CoverPatterns],
    CoverMatchSpecs.

-spec determine_cover_patterns_locally() -> CoverPatterns when
      CoverPatterns :: [any(), ...].
%% @doc Computes the cover-specific instructions patterns.
%%
%% To achieve this, the function uses a very basic module (see
%% `priv/horus_cover_helper.erl').
%%
%% First, it compiles the module with `compile' and extracts its single
%% function using Horus. Then it compiles the same module with `cover' and
%% extracts the function again.
%%
%% It then compares the bodies of the two copies of the same function to
%% determine the instructions added by `cover'.
%%
%% This list of instructions is converted to ETS match patterns. The patterns
%% will be used in {@link isolate_cover_instructions/2}.
%%
%% @private

determine_cover_patterns_locally() ->
    %% Compile the `horus_cover_helper' module at runtime. It is located in
    %% the `priv' directory.
    %%
    %% We can't use an on-disk module in `src' because Rebar will
    %% cover-compile it out-of-the-box when executing the testsuite and thus
    %% we can't compare it with the copy we cover-compile ourselves.
    PrivDir = code:priv_dir(horus),
    SourceFile = filename:join(PrivDir, "horus_cover_helper.erl"),
    CompileOpts = [binary, return_errors],
    {ok, Module, Beam} = compile:file(SourceFile, CompileOpts),
    ?assertEqual(?COVER_HELPER_MOD, Module),

    %% We load the module to create the anonymous function reference and
    %% extract the standalone function.
    {module, Module} = code:load_binary(Module, SourceFile, Beam),
    ok = horus:override_object_code(Module, Beam),
    Fun = fun Module:entry_point/0,
    ShouldProcessFunction = fun
                                (M, _, _, _) when M =:= Module -> true;
                                (_, _, _, _)                   -> false
                            end,
    HorusOpts = #{debug_info => true,
                  add_module_info => false,
                  should_process_function => ShouldProcessFunction},
    StandaloneFun1 = horus:to_standalone_fun(Fun, HorusOpts),
    ok = horus:forget_overridden_object_code(Module),

    %% We do the same with `cover'.
    {ok, Module} = cover:compile_module(SourceFile),
    StandaloneFun2 = horus:to_standalone_fun(Fun, HorusOpts),
    RegularBody = get_entrypoint_body(StandaloneFun1),
    CoveredBody = get_entrypoint_body(StandaloneFun2),

    %% We now compare the two function bodies to determine cover-specific
    %% instructions.
    do_determine_cover_patterns(CoveredBody, RegularBody).

get_entrypoint_body(StandaloneFun) ->
    %% We skip the function header until we reach the label of the body.
    Asm = get_entrypoint_asm(StandaloneFun),
    Body = lists:dropwhile(
             fun
                 ({label, 2}) -> false;
                 (_)          -> true
             end, Asm),
    Body.

get_entrypoint_asm(#horus_fun{debug_info = #{asm := Asm}}) ->
    {_, _, _, Functions1, _} = Asm,
    Entrypoint = hd(Functions1),
    EntrypointAsm = element(5, Entrypoint),
    EntrypointAsm.

do_determine_cover_patterns(CoveredBody, RegularBody) ->
    {CoveredBody1, RegularBody1} = eliminate_common_start(
                                     CoveredBody, RegularBody),
    CoveredStart = get_cover_specific_start(CoveredBody1, RegularBody1, []),
    %% We ensure the cover-specific instructions were found. If the result is
    %% an empty list, it means we compared two identical standalone functions,
    %% meaning something went wrong with the compilation.
    case CoveredStart of
        [] ->
            ?horus_misuse(
               failed_to_determine_cover_specific_instructions,
               #{});
        _ ->
            CoveredStart
    end.

eliminate_common_start(
  [Instruction | CoveredRest], [Instruction | RegularRest]) ->
    eliminate_common_start(CoveredRest, RegularRest);
eliminate_common_start(CoveredBody, RegularBody) ->
    {CoveredBody, RegularBody}.

get_cover_specific_start(
  [Instruction | _CoveredRest], [Instruction | _RegularRest],
  CoveredStart) ->
    CoveredStart1 = lists:reverse(CoveredStart),
    CoverPatterns = instructions_to_patterns(CoveredStart1),
    CoverPatterns;
get_cover_specific_start(
  [Instruction | CoveredRest], RegularRest, CoveredStart) ->
    CoveredStart1 = [Instruction | CoveredStart],
    get_cover_specific_start(CoveredRest, RegularRest, CoveredStart1).

instructions_to_patterns(CoveredStart) ->
    lists:filtermap(
      fun
          ({move, {literal, {cover, _}}, _}) ->
              {true, {move, {literal, {cover, '_'}}, '_'}};
          ({call_ext, _, {extfunc, _, _, _}}) ->
              {true, {call_ext, '_', {extfunc, '_', '_', '_'}}};
          ({move, _, _}) ->
              {true, {move, '_', '_'}};
          ({allocate, _, _}) ->
              false;
          ({line, _}) ->
              false
      end, CoveredStart).

-spec determine_cover_patterns_remotely(Node) -> CoverPatterns when
      Node :: node(),
      CoverPatterns :: [any(), ...].
%% @doc Calls `Node' to determine the cover-specific instructions.

determine_cover_patterns_remotely(Node) ->
    erpc:call(Node, ?MODULE, determine_cover_patterns_locally, []).
