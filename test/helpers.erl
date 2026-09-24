%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(helpers).

-include_lib("stdlib/include/assert.hrl").

-include("test/helpers.hrl").

-export([ensure_not_optimized/1,
         start_epmd/0,
         start_n_nodes/2]).

-export([horus_to_standalone_fun/1, horus_to_standalone_fun/2,
         horus_exec/2]).

-spec ensure_not_optimized(Value) -> Value when
      Value :: term().
%% @doc Makes sure the given value is not optimized out by the compiler.
%%
%% The compiler is smart enough to optimize away many instructions by
%% inspecting types and values. This function confuses the compiler by sending
%% and receiving the value.

ensure_not_optimized(Value) ->
    self() ! Value,
    receive Msg -> Msg end.

start_epmd() ->
    RootDir = code:root_dir(),
    ErtsVersion = erlang:system_info(version),
    ErtsDir = lists:flatten(io_lib:format("erts-~ts", [ErtsVersion])),
    EpmdPath0 = filename:join([RootDir, ErtsDir, "bin", "epmd"]),
    EpmdPath = case os:type() of
                   {win32, _} -> EpmdPath0 ++ ".exe";
                   _          -> EpmdPath0
               end,
    Port = erlang:open_port(
             {spawn_executable, EpmdPath},
             [{args, ["-daemon"]}]),
    erlang:port_close(Port),
    ok.

start_n_nodes(NamePrefix, Count) ->
    io:format("Start ~b Erlang nodes:~n", [Count]),
    Nodes = [begin
                 Name = lists:flatten(
                          io_lib:format(
                            "~s-~s-~b", [?MODULE, NamePrefix, I])),
                 io:format("- ~s~n", [Name]),
                 start_erlang_node(Name)
             end || I <- lists:seq(1, Count)],
    io:format("Started nodes: ~p~n", [[Node || {Node, _Peer} <- Nodes]]),

    CodePath = code:get_path(),
    lists:foreach(
      fun({Node, _Peer}) ->
              erpc:call(Node, code, add_pathsz, [CodePath])
      end, Nodes),

    %% We add all nodes to the test coverage report.
    CoveredNodes = [Node || {Node, _Peer} <- Nodes],
    {ok, _} = cover:start(CoveredNodes),

    CoveredNodes.

-if(?OTP_RELEASE >= 25).
start_erlang_node(Name) ->
    Name1 = list_to_atom(Name),
    {ok, Peer, Node} = peer:start(#{name => Name1,
                                    wait_boot => infinity}),
    {Node, Peer}.
-else.
start_erlang_node(Name) ->
    Name1 = list_to_atom(Name),
    Options = [{monitor_master, true}],
    {ok, Node} = ct_slave:start(Name1, Options),
    {Node, Node}.
-endif.

horus_to_standalone_fun(Fun) ->
    case get_remote_extractor() of
        none ->
            horus:to_standalone_fun(Fun);
        ServerRef ->
            FunInfo = erlang:fun_info(Fun),
            Mod = proplists:get_value(module, FunInfo),
            _ = peer:call(
                  ServerRef,
                  Mod, module_info, [], infinity),
            peer:call(
              ServerRef,
              horus, to_standalone_fun, [Fun], infinity)
    end.

horus_to_standalone_fun(Fun, Options) ->
    case get_remote_extractor() of
        none ->
            horus:to_standalone_fun(Fun, Options);
        ServerRef ->
            FunInfo = erlang:fun_info(Fun),
            Mod = proplists:get_value(module, FunInfo),
            _ = peer:call(
                  ServerRef,
                  Mod, module_info, [], infinity),
            peer:call(
              ServerRef,
              horus, to_standalone_fun, [Fun, Options], infinity)
    end.

horus_exec(StandaloneFun, Options) ->
    case get_remote_executor() of
        none ->
            case get_remote_extractor() of
                none ->
                    horus:exec(StandaloneFun, Options);
                ServerRef ->
                    peer:call(
                      ServerRef,
                      horus, exec, [StandaloneFun, Options], infinity)
            end;
        ServerRef ->
            peer:call(
              ServerRef,
              horus, exec, [StandaloneFun, Options], infinity)
    end.

-define(EXTRACTOR_KEY, horus_test_extractor).
-define(EXECUTOR_KEY, horus_test_executor).

get_remote_extractor() ->
    get_remote_env(?EXTRACTOR_KEY).

get_remote_executor() ->
    get_remote_env(?EXECUTOR_KEY).

get_remote_env(?EXTRACTOR_KEY = Key) ->
    start_remote_env("HORUS_TEST_EXTRACTOR", Key);
get_remote_env(?EXECUTOR_KEY = Key) ->
    start_remote_env("HORUS_TEST_EXECUTOR", Key).

start_remote_env(EnvVar, Key) ->
    try
        persistent_term:get(Key)
    catch
        error:badarg ->
            case os:getenv(EnvVar) of
                false ->
                    set_no_remote_env(Key);
                ErlPath ->
                    case filelib:is_regular(ErlPath) of
                        true  -> do_start_remote_env(Key, ErlPath);
                        false -> set_no_remote_env(Key)
                    end
            end
    end.

set_no_remote_env(Key) ->
    RemoteEnv = none,
    persistent_term:put(Key, RemoteEnv),
    RemoteEnv.

do_start_remote_env(Key, ErlPath) ->
    Lock = {?FUNCTION_NAME, self()},
    global:set_lock(Lock, [node()]),
    try
        persistent_term:get(Key)
    catch
        error:badarg ->
            io:format(
              standard_error,
              "[use \"~s\" to ~s functions]~n",
              [ErlPath,
               case Key of
                   ?EXTRACTOR_KEY -> "extract";
                   ?EXECUTOR_KEY  -> "execute"
               end]),
            Options = #{peer_down => crash,
                        connection => standard_io,
                        exec => ErlPath},
            {ok, RemoteEnv, _Node} = peer:start_link(Options),
            ok = prepare_horus_on_remote_env(Key, RemoteEnv),
            persistent_term:put(Key, RemoteEnv),
            ok = validate_remote_env(Key),
            RemoteEnv
    after
        global:del_lock(Lock, [node()])
    end.

prepare_horus_on_remote_env(Key, RemoteEnv) ->
    HorusDir = code:lib_dir(horus),
    case Key of
        ?EXTRACTOR_KEY ->
            EbinDir = filename:join(HorusDir, "ebin"),
            TestDir = filename:join(HorusDir, "test"),
            ok = peer:call(
                   RemoteEnv, code, add_pathsa, [[EbinDir, TestDir]]),
            ok;
        ?EXECUTOR_KEY ->
            {ok, Cwd} = file:get_cwd(),
            OutDir = filename:join(Cwd, "_ebin_" ++ atom_to_list(Key)),
            AppFileSrc = filename:join([HorusDir, "ebin", "horus.app"]),
            AppFileDst = filename:join([OutDir, "horus.app"]),
            _ = file:make_dir(OutDir),
            {ok, _} = file:copy(AppFileSrc, AppFileDst),
            ok = compile_horus_on_remote_env(RemoteEnv, OutDir),
            true = peer:call(RemoteEnv, code, add_patha, [OutDir])
    end,
    ok = start_horus_on_remote_env(RemoteEnv),
    OtpRel = peer:call(RemoteEnv, erlang, system_info, [otp_release]),
    io:format(
      standard_error,
      "[~s node ready, version ~s]~n",
      [case Key of
           ?EXTRACTOR_KEY -> "extractor";
           ?EXECUTOR_KEY  -> "executor"
       end,
       OtpRel]),
    ok.

compile_horus_on_remote_env(RemoteEnv, OutDir) ->
    SrcFiles = filelib:wildcard(filename:join("src", "*.erl")),
    Options = [debug_info,
               warnings_as_errors,
               {outdir, OutDir}],
    lists:foreach(
      fun(SrcFile) ->
              ok = compile_file_on_remote_env(RemoteEnv, SrcFile, Options)
      end, SrcFiles),
    ok.

compile_file_on_remote_env(RemoteEnv, SrcFile, Options) ->
    {ok, _} = peer:call(RemoteEnv, compile, file, [SrcFile, Options]),
    ok.

start_horus_on_remote_env(RemoteEnv) ->
    {ok, _} = peer:call(RemoteEnv, application, ensure_all_started, [horus]),
    ok.

validate_remote_env(Key) ->
    RemoteEnv = get_remote_env(Key),
    OtpRel = peer:call(RemoteEnv, erlang, system_info, [otp_release]),
    StandaloneFun = ?make_standalone_fun(erlang:system_info(otp_release)),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(
       OtpRel,
       case Key of
           ?EXTRACTOR_KEY ->
               peer:call(RemoteEnv, horus, exec, [StandaloneFun, []]);
           ?EXECUTOR_KEY ->
               helpers:horus_exec(StandaloneFun, [])
       end),
    ok.
