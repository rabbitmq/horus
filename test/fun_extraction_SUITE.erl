%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2022-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(fun_extraction_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         fun_extraction_works_in_rebar3_ct/1]).

all() ->
    [fun_extraction_works_in_rebar3_ct].

groups() ->
    [].

init_per_suite(Config) ->
    ok = cth_log_redirect:handle_remote_events(true),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(fun_extraction_works_in_rebar3_ct, Config) ->
    ModifiedMods = code:modified_modules(),
    HorusFunModified = lists:member(horus, ModifiedMods),
    ct:pal(
      "Modified modules: ~p~n"
      "`horus` recompiled: ~s",
      [ModifiedMods, HorusFunModified]),
    case HorusFunModified of
        true ->
            Config;
        false ->
            {skip,
             "Horus not recompiled at runtime by Rebar; "
             "test conditions not met"}
    end;
init_per_testcase(_Testcase, Config) ->
    Config.

end_per_testcase(_Testcase, _Config) ->
    ok.

fun_extraction_works_in_rebar3_ct(_Config) ->
    %% Rebar3 uses `cth_readable' to offer a cleaner output of common_test on
    %% stdout. As part of that, it recompiles the entire application on-the-fly
    %% with the `cth_readable_transform' parse_transform. This means that the
    %% module on disk and the one loaded in memory are different. See
    %% `horus:is_recompiled_module_acceptable()'.
    %%
    %% This testcase depends on that behavior (see `init_per_testcase').
    %%
    %% To test `horus', it needs to get an anonymous function created by
    %% the application (not the testsuite) as the top-level function to
    %% extract.
    %%
    %% The `InnerFun' is just here to have an argument to pass to
    %% `horus:to_actual_arg()'. `horus:to_actual_arg()' is the
    %% function we call to get that anonymous function from the application.
    InnerFun = fun() -> true end,
    InnerStandaloneFun = horus:to_standalone_fun(InnerFun),
    Options = #{ensure_instruction_is_permitted =>
                fun(_) -> ok end,
                should_process_function =>
                fun
                    (code, _, _, _)   -> false;
                    (erlang, _, _, _) -> false;
                    (lists, _, _, _)  -> false;
                    (maps, _, _, _)   -> false;
                    (horus, exec, _, _) -> false;
                    (_Mod, _, _, _)   -> true
                end},
    OuterFun = horus:to_actual_arg(InnerStandaloneFun),

    %% Without `horus:is_recompiled_module_acceptable()', `horus'
    %% would fail the extraction below because of the two copies of the
    %% `horus' module.
    OuterStandaloneFun = horus:to_standalone_fun(OuterFun, Options),

    %% We execute the extracted function just as an extra check, but this is
    %% not critical for this testcase.
    ?assertEqual(
       OuterFun(),
       horus:exec(OuterStandaloneFun, [])),

    ok.
