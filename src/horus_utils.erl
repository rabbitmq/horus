%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

%% @private

-module(horus_utils).

-export([should_process_module/1,
         init_list_of_modules_to_skip/0,
         clear_list_of_modules_to_skip/0]).

%% -------------------------------------------------------------------
%% Helpers to standalone function extraction.
%% -------------------------------------------------------------------

-define(PT_MODULES_TO_SKIP, {horus, skipped_modules_in_code_collection}).

-spec should_process_module(Module) -> Collect when
      Module :: module(),
      Collect :: boolean().
%% @doc Indicates if the code from `Module' should be processed/collected.

should_process_module(Module) ->
    try
        Modules = persistent_term:get(?PT_MODULES_TO_SKIP),
        not maps:is_key(Module, Modules)
    catch
        error:badarg ->
            ok = init_list_of_modules_to_skip(),
            should_process_module(Module)
    end.

-spec init_list_of_modules_to_skip() -> ok.
%% @doc Initializes a list of modules from Erlang standalone library that
%% should not be collected.

init_list_of_modules_to_skip() ->
    SkippedApps0 = [erts, kernel, stdlib, horus],
    SkippedApps1 = application:get_env(horus, skip_collection_froms_apps, []),
    SkippedApps = lists:usort(SkippedApps0 ++ SkippedApps1),
    Modules = lists:foldl(
                fun(App, Modules0) ->
                        _ = application:load(App),
                        Mods = case application:get_key(App, modules) of
                                   {ok, List} -> List;
                                   undefined  -> []
                               end,
                        lists:foldl(
                          fun(Mod, Modules1) -> Modules1#{Mod => true} end,
                          Modules0, Mods)
                end, #{}, SkippedApps),
    persistent_term:put(?PT_MODULES_TO_SKIP, Modules),
    ok.

-spec clear_list_of_modules_to_skip() -> ok.
%% @doc Clears the cached list of modules to not collect.

clear_list_of_modules_to_skip() ->
    _ = persistent_term:erase(?PT_MODULES_TO_SKIP),
    ok.
