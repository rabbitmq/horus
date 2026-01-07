%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(failing_funs).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

-dialyzer([{no_return,
            [stacktrace_test/0]}]).

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
    Fun = fun() -> failing_fun() end,
    StandaloneFun = horus:to_standalone_fun(Fun, #{debug_info => true}),
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
            %% When comparing the stacktraces, we only consider the part
            %% inside the anonymous function. That's why we take all frames
            %% while they belong to the anonymous function and the function is
            %% calls.
            %%
            %% For `Stacktrace1', we will stop before the frame calling the
            %% anonymous function (i.e. this test function). For
            %% `Stacktrace2', we will stop before the `horus:exec/2' call.
            Pred = fun({Module, Name, _, _}) ->
                           Module =:= ?MODULE andalso
                           Name =/= ?FUNCTION_NAME
                   end,
            Stacktrace1a = lists:takewhile(Pred, Stacktrace1),
            Stacktrace2a = lists:takewhile(Pred, Stacktrace2),
            ?assertNotEqual([], Stacktrace1a),
            ?assertNotEqual([], Stacktrace2a),
            ?assertEqual(Stacktrace1a, Stacktrace2a)
    end.

-spec failing_fun() -> no_return().

failing_fun() ->
    error(failure).
