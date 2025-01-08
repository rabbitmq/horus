%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(erlang_binaries).

-include_lib("eunit/include/eunit.hrl").

-include("test/helpers.hrl").

-dialyzer([{no_match,
            [matches_type/2,
             trim_leading_dash3/2]}]).

concat_binaries_test() ->
    Bin = helpers:ensure_not_optimized(<<"a">>),
    StandaloneFun = ?make_standalone_fun(
                       <<Bin/binary, "_", Bin/binary, "_", Bin/binary>>),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(<<"a_a_a">>, horus:exec(StandaloneFun, [])).

bs_match_test() ->
    List = [{'apply-to', <<"queues">>}],
    StandaloneFun = ?make_standalone_fun(
                       begin
                           matches_type(
                             queue, proplists:get_value('apply-to', List)),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

matches_type(exchange, <<"exchanges">>) -> true;
matches_type(queue,    <<"queues">>)    -> true;
matches_type(exchange, <<"all">>)       -> true;
matches_type(queue,    <<"all">>)       -> true;
matches_type(_,        _)               -> false.

bitstring_init_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           <<25:7/integer>> = encode_integer(25),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

encode_integer(Length) ->
    <<Length:7/integer>>.

bs_match_1_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           {<<"2022">>, <<"02">>, <<"02">>} =
                           parse_date(<<"2022-02-02">>),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

parse_date(
  <<Year:4/bytes, $-, Month:2/bytes, $-, Day:2/bytes, _Rest/binary>>) ->
    {Year, Month, Day}.

bs_match_2_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           {[], <<1, 2, 3, 4, 5>>} =
                           parse_float(<<".", 1, 2, 3, 4, 5>>),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

parse_float(<<".", Rest/binary>>) ->
    parse_digits(Rest);
parse_float(Bin) ->
    {[], Bin}.

parse_digits(Bin) ->
    parse_digits(Bin, []).

parse_digits(<<Digit/integer, Rest/binary>>, Acc)
  when is_integer(Digit) andalso Digit >= 48 andalso Digit =< 57 ->
    parse_digits(Rest, [Digit | Acc]);
parse_digits(Rest, Acc) ->
    {lists:reverse(Acc), Rest}.

%% This set of parse_float, parse_digits, etc. is the same as the above
%% functions and test case, except that the intermediary function
%% `parse_digits/2' introduces new bindings that change the arity, to
%% ensure we are not hard-coding an arity.
parse_float2(<<".", Rest/binary>>) ->
    parse_digits2([], Rest);
parse_float2(Bin) ->
    {[], Bin}.

parse_digits2(Foo, Bin) ->
    parse_digits2(Foo, [], Bin).

parse_digits2(Foo, Bar, Bin) ->
    parse_digits2(Foo, Bar, Bin, []).

parse_digits2(Foo, Bar, <<Digit/integer, Rest/binary>>, Acc)
  when is_integer(Digit) andalso Digit >= 48 andalso Digit =< 57 ->
    parse_digits2(Foo, Bar, Rest, [Digit | Acc]);
parse_digits2(_Foo, _Bar, Rest, Acc) ->
    {lists:reverse(Acc), Rest}.

bs_match_3_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           {[], <<1, 2, 3, 4, 5>>} =
                           parse_float2(<<".", 1, 2, 3, 4, 5>>),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

%% The compiler determines that this clause will always match because this
%% function is not exported and is only called with a compile-time binary
%% matching the pattern. As a result, the instruction for this match is
%% `bs_start_match4'
trim_leading_dash1(<<$-, Rest/binary>>) -> trim_leading_dash1(Rest);
trim_leading_dash1(Binary)              -> Binary.

%% This is the same function but we'll give it a non-binary argument in
%% the test case to avoid the `bs_start_match4' optimization. Instead
%% the compiler uses a `{test,bs_start_match3,..}` instruction.
trim_leading_dash2(<<$-, Rest/binary>>) -> trim_leading_dash2(Rest);
trim_leading_dash2(Binary)              -> Binary.

%% Again, effectively the same function but to fix compilation for this
%% case we need to determine the correct arity to mark as accepting
%% a match context, so we should test a case where the binary match
%% is done in another argument.
trim_leading_dash3(Arg, <<$-, Rest/binary>>) -> trim_leading_dash3(Arg, Rest);
trim_leading_dash3(_Arg, Binary)             -> Binary.

bs_match_accepts_match_context_test() ->
    StandaloneFun = ?make_standalone_fun(
                       begin
                           <<"5">> = trim_leading_dash1(<<"-5">>),
                           <<"5">> = trim_leading_dash2(<<"-5">>),
                           "-5" = trim_leading_dash2("-5"),
                           "-5" = trim_leading_dash3([], "-5"),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

bs_get_float_test() ->
    FloatBin = helpers:ensure_not_optimized(<<3.14/float>>),
    StandaloneFun = ?make_standalone_fun(
                       begin
                           3.14 = match_float(FloatBin),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

match_float(<<Float/float>>) ->
    Float.

type_inference_for_test_arity_instruction_test() ->
    self() ! {text, false},
    TextFrame = receive TextMsg -> TextMsg end,
    self() ! {binary, true},
    BinaryFrame = receive BinaryMsg -> BinaryMsg end,
    StandaloneFun = ?make_standalone_fun(
                       begin
                           <<0:1>> = encode_frame(TextFrame),
                           <<1:1>> = encode_frame(BinaryFrame),
                           ok
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(ok, horus:exec(StandaloneFun, [])).

encode_frame(Frame)
    when is_tuple(Frame) andalso
        (element(1, Frame) =:= text orelse
         element(1, Frame) =:= binary) ->
    <<(encode_fin(Frame))/bitstring>>.

encode_fin({text, false})   -> <<0:1/integer>>;
encode_fin({binary, false}) -> <<0:1/integer>>;
encode_fin(_)               -> <<1:1/integer>>.

bit_string_comprehension_expression_test() ->
    Data = crypto:strong_rand_bytes(128),
    <<Mask:32/integer>> = crypto:strong_rand_bytes(4),
    StandaloneFun = ?make_standalone_fun(
                       begin
                           <<<<(Part bxor Mask):32/integer>>
                             || <<Part:32/integer>> <= Data>>
                       end),
    ?assertStandaloneFun(StandaloneFun),
    ?assertEqual(
       <<<<(Part bxor Mask):32/integer>>
         || <<Part:32/integer>> <= Data>>,
       horus:exec(StandaloneFun, [])).

bitstring_flags_test() ->
    LittleSignedBin = helpers:ensure_not_optimized(
                        <<-42:4/little-signed-integer-unit:8>>),
    LittleUnsignedBin = helpers:ensure_not_optimized(
                          <<42:4/little-unsigned-integer-unit:8>>),
    BigSignedBin = helpers:ensure_not_optimized(
                     <<-42:4/big-signed-integer-unit:8>>),
    BigUnsignedBin = helpers:ensure_not_optimized(
                       <<42:4/big-unsigned-integer-unit:8>>),
    Decode = ?make_standalone_fun(
               begin
                   {match_bitstring_flags(
                      {little_signed, LittleSignedBin}),
                    match_bitstring_flags(
                      {little_unsigned, LittleUnsignedBin}),
                    match_bitstring_flags(
                      {big_signed, BigSignedBin}),
                    match_bitstring_flags(
                      {big_unsigned, BigUnsignedBin})}
               end),
    ?assertStandaloneFun(Decode),
    ?assertEqual({-42, 42, -42, 42}, horus:exec(Decode, [])).

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
