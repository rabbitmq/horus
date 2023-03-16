%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(failing_funs).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

throw_test() ->
    StandaloneFun = ?make_standalone_fun(
                      begin
                          throw(failure)
                      end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertThrow(failure, horus:exec(StandaloneFun, [])).

error_test() ->
    StandaloneFun = ?make_standalone_fun(
                      begin
                          error(failure)
                      end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertError(failure, horus:exec(StandaloneFun, [])).

exit_test() ->
    StandaloneFun = ?make_standalone_fun(
                      begin
                          exit(failure)
                      end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertExit(failure, horus:exec(StandaloneFun, [])).

stacktrace_test() ->
    Fun = fun() -> error(failure) end,
    StandaloneFun = horus:to_standalone_fun(Fun),
    ?assertStandaloneFun(StandaloneFun),
    Stacktrace1 = try
                      Fun()
                  catch
                      error:failure:Stacktrace ->
                          Stacktrace
                  end,
    try
        horus:exec(StandaloneFun, [])
    catch
        error:failure:Stacktrace2 ->
            %% TODO: Rewrite stacktraces to point to original MFAs too.
            {_, _, _, Loc1} = hd(Stacktrace1),
            {_, _, _, Loc2} = hd(Stacktrace2),
            ?assertEqual(Loc1, Loc2)
    end.
