%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(experiment).

-include_lib("eunit/include/eunit.hrl").

% a_test() ->
%     Fun = fun(A) when is_list(A) -> ok end,
%     Ret = horus_beam_utils:get_fun_abstract_code(Fun),
%     logger:alert("Fun = ~p", [Ret]).

bitstring_flags_test() ->
    LittleSignedBin = helpers:ensure_not_optimized(
                        <<-42:4/little-signed-integer-unit:8>>),
    LittleUnsignedBin = helpers:ensure_not_optimized(
                          <<42:4/little-unsigned-integer-unit:8>>),
    BigSignedBin = helpers:ensure_not_optimized(
                     <<-42:4/big-signed-integer-unit:8>>),
    BigUnsignedBin = helpers:ensure_not_optimized(
                       <<42:4/big-unsigned-integer-unit:8>>),
    Fun = fun() ->
                  {match_bitstring_flags(
                     {little_signed, LittleSignedBin}),
                   match_bitstring_flags(
                     {little_unsigned, LittleUnsignedBin}),
                   match_bitstring_flags(
                     {big_signed, BigSignedBin}),
                   match_bitstring_flags(
                     {big_unsigned, BigUnsignedBin})}
          end,
    % Ret = horus_beam_utils:get_fun_abstract_code(Fun),
    try
        Ret = horus2:to_standalone_fun(Fun),
        logger:alert("Fun = ~p", [Ret]),
        {ok, StandaloneFun} = Ret,
        Ret1 = horus:exec(StandaloneFun, []),
        logger:alert("Fun ret = ~p", [Ret1]),
        Ret1
    catch
        C:R:S ->
            logger:alert("~p:~p:~p", [C, R, S]),
            erlang:raise(C, R, S)
    end.

match_bitstring_flags(
  {little_signed, <<N:4/little-signed-integer-unit:8>>}) ->
    N;
match_bitstring_flags(
  {big_signed, <<N:4/big-signed-integer-unit:8>>}) ->
    N;
match_bitstring_flags(
  {little_unsigned, <<N:4/little-unsigned-integer-unit:8>>}) ->
    N;
match_bitstring_flags(
  {big_unsigned, <<N:4/big-unsigned-integer-unit:8>>}) ->
    N.
