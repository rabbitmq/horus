%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

%% @private

-module(horus_utils).

-export([is_module_loaded/1,
         should_process_module/1,
         init_list_of_modules_to_skip/0,
         clear_list_of_modules_to_skip/0]).

-spec is_module_loaded(Module) -> IsLoaded when
      Module :: module(),
      IsLoaded :: boolean().
%% @doc Indicates if a module is loaded or not.
%%
%% For Erlang/OTP up to 25, it is an alternative to `code:is_loaded/1' because
%% it is a synchronous call to the code server in these versions.
%%
%% For Erlang/OTP 26+, this is a simply wrapper.
%%
%% @end

%% We use a compile-time check instead of a runtime check because if the
%% application is compiled with Erlang/OTP 25, `is_module_loaded/1' works
%% equally well on Erlang/OTP 26.
%%
%% If the application is compiled with Erlang/OTP26, then it won't load on
%% Erlang/OTP 25 anyway.

-if(?OTP_RELEASE >= 26).
is_module_loaded(Module) ->
    %% Starting from Erlang/OTP 26, this is a query of a protected ETS table.
    case code:is_loaded(Module) of
        {file, _} -> true;
        false     -> false
    end.
-else.
is_module_loaded(Module) ->
    %% Up to Erlang/OTP 25, this is a synchronous call to the Code server. This
    %% is a contention point, so let's use the undocumented
    %% `erlang:get_module_info/2' BIF (which is behind any
    %% `Module:module_info/1') to test the presence of the module.
    try
        _ = erlang:get_module_info(Module, md5),
        true
    catch
        error:badarg ->
            false
    end.
-endif.

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
