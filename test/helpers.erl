%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(helpers).

-export([ensure_not_optimized/1,
         start_epmd/0,
         start_n_nodes/2]).

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
