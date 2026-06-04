%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_asm_utils).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("src/horus_error.hrl").

-export([disassemble/1]).

-type beam_instr() :: atom() | tuple().

-type asm() :: list().

%% -------------------------------------------------------------------
%% Taken from lib/compiler/src/beam_disasm.hrl,
%% commit 7b3ffa5bb72a2ba84b07fb8a98d755216e78fa79
-record(function, {name      :: atom(),
                   arity     :: byte(),
                   entry     :: beam_lib:label(),    %% unnecessary ?
                   code = [] :: [beam_instr()]}).

-record(beam_file, {module               :: module(),
                    labeled_exports = [] :: [beam_lib:labeled_entry()],
                    attributes      = [] :: [beam_lib:attrib_entry()],
                    compile_info    = [] :: [beam_lib:compinfo_entry()],
                    code            = [] :: [#function{}]}).

%% The following records are used to store the decoded "Line" beam chunk. They
%% are used while processing `line' instructions to restore the correct
%% location. This is needed so that exception stacktraces point to real
%% locations in source files.

-record(lines, {item_count,
                items = [],
                name_count,
                names = [],
                location_size}).

-record(line, {name_index,
               location}).

%% The following record is used to decode tagged types.

-record(tag, {tag,
              size,
              word_value,
              ptr_value}).

-export_type([asm/0]).

-spec disassemble(Beam) -> Asm when
      Beam :: horus_beam_utils:beam(),
      Asm :: horus_asm_utils:asm().

disassemble(Beam) ->
    BeamFileRecord = beam_disasm:file(Beam),
    #beam_file{
       module = Module,
       labeled_exports = LabeledExports,
       attributes = Attributes,
       code = FunctionRecords} = BeamFileRecord,
    Exports = [{FunName, Arity} || {FunName, Arity, _} <- LabeledExports],
    Functions1 = [[{function, FunName, Arity, EntryLabel} | Instructions]
                  || #function{name = FunName,
                               arity = Arity,
                               entry = EntryLabel,
                               code = Instructions} <- FunctionRecords],
    Functions2 = lists:flatten(Functions1),
    Lines = decode_line_chunk(Module, Beam),
    Functions3 = fix_line_instructions(Functions2, Lines),
    [{module, Module},
     {exports, Exports},
     {attributes, Attributes}
     | Functions3].

fix_line_instructions(Instructions, #lines{items = Items, names = Names}) ->
    lists:map(
      fun
          ({line, Index}) ->
              #line{name_index = NameIndex,
                    location = Location} = lists:nth(Index + 1, Items),
              Name = lists:nth(NameIndex + 1, Names),
              {line, [{location, Name, Location}]};
          (Instruction) ->
              Instruction
      end, Instructions).

%% The "Line" beam chunk decoding is based on the equivalent C code in ERTS.
%% See: erts/emulator/beam/beam_file.c, parse_line_chunk().
%%
%% The opposite encoding function is inside the compiler.
%% See: compiler/src/beam_asm.erl, build_line_table().

-define(CHAR_BIT, 8).
-define(sizeof_Sint16, 2).
-define(sizeof_Sint32, 4).
-define(sizeof_SWord, 4).

%% See erts/emulator/beam/big.h
%% Here, we assume a 64bit architecture with 4-byte integers.
-define(_IS_SSMALL32(X), true).
-define(IS_SSMALL(X), ?_IS_SSMALL32(X)).

decode_line_chunk(Module, Beam) ->
    case beam_lib:chunks(Beam, ["Line"]) of
        {ok, {Module, [{"Line", Chunk}]}} ->
            decode_line_chunk1(Module, Chunk);
        _ ->
            undefined
    end.

decode_line_chunk1(Module, Chunk) ->
    decode_line_chunk_version(Module, Chunk).

decode_line_chunk_version(
  Module,
  <<Version:32/integer,
    Rest/binary>>)
  when Version =:= 0 ->
    %% The original C code makes an assertion that the version is 0, thus the
    %% guard expression above.
    decode_line_chunk_counts_and_flags(Module, Rest).

decode_line_chunk_counts_and_flags(
  Module,
  <<_Flags:32/integer,
    _InstrCount:32/integer,
    ItemCount:32/integer,
    NameCount:32/integer,
    Rest/binary>>) ->
    UndefinedLocation = #line{name_index = 0,
                              location = 0},
    ModuleFilename = atom_to_list(Module) ++ ".erl",
    NameCount1 = NameCount + 1,
    ItemCount1 = ItemCount + 1,
    LocationSize = if
                       NameCount1 > 1 -> ?sizeof_Sint32;
                       true           -> ?sizeof_Sint16
                   end,
    Lines = #lines{item_count = ItemCount1,
                   items = [UndefinedLocation],
                   name_count = NameCount1,
                   names = [ModuleFilename],
                   location_size = LocationSize},
    NameIndex = 0,
    I = 1,
    decode_line_chunk_items(Rest, I, NameIndex, Lines).

decode_line_chunk_items(
  Rest, I, NameIndex,
  #lines{item_count = ItemCount,
         items = Items,
         location_size = LocationSize} = Lines)
  when I < ItemCount ->
    {Tag, Rest1} = read_tagged(Rest),
    case Tag of
        #tag{tag = tag_a, word_value = WordValue} ->
            NameIndex1 = WordValue,
            decode_line_chunk_items(Rest1, I, NameIndex1, Lines);
        #tag{tag = tag_i, size = 0, word_value = WordValue}
          when WordValue >= 0 ->
            LocationSize1 = if
                                WordValue > 16#FFFF -> ?sizeof_Sint32;
                                true                -> LocationSize
                            end,
            Item = #line{name_index = NameIndex,
                         location = WordValue},
            Lines1 = Lines#lines{items = Items ++ [Item],
                                 location_size = LocationSize1},
            decode_line_chunk_items(Rest1, I + 1, NameIndex, Lines1)
    end;
decode_line_chunk_items(
  Rest, I, _NameIndex,
  #lines{item_count = ItemCount} = Lines) when I =:= ItemCount ->
    decode_line_chunk_names(Rest, 1, Lines).

decode_line_chunk_names(
  <<NameLength:16/integer, Name:NameLength/binary, Rest/binary>>,
  I,
  #lines{name_count = NameCount,
         names = Names} = Lines)
  when I < NameCount ->
    Name1 = unicode:characters_to_list(Name),
    Names1 = Names ++ [Name1],
    Lines1 = Lines#lines{names = Names1},
    decode_line_chunk_names(Rest, I + 1, Lines1);
decode_line_chunk_names(<<>>, I, #lines{name_count = NameCount} = Lines)
  when I =:= NameCount ->
    Lines.

%% See: erts/emulator/beam/beam_file.c, beamreader_read_tagged().

read_tagged(
  <<LenCode:8/unsigned-integer,
    Rest/binary>>) ->
    Tag = decode_tag(LenCode band 16#07),
    if
        LenCode band 16#08 =:= 0 ->
            WordValue = LenCode bsr 4,
            Size = 0,
            {#tag{tag = Tag,
                  word_value = WordValue,
                  size = Size},
             Rest};
        LenCode band 16#10 =:= 0 ->
            <<ExtraByte:8/unsigned-integer, Rest1/binary>> = Rest,
            WordValue = ((LenCode bsr 5) bsl 8) bor ExtraByte,
            Size = 0,
            {#tag{tag = Tag,
                  word_value = WordValue,
                  size = Size},
             Rest1};
        true ->
            LenCode1 = LenCode bsr 5,
            {Count, Rest1} = if
                                 LenCode1 < 7 ->
                                     SizeBase = 2,
                                     {LenCode1 + SizeBase, Rest};
                                 true ->
                                     SizeBase = 9,
                                     {#tag{tag = tag_u,
                                           word_value = UnpackedSize},
                                      R1} = read_tagged(Rest),
                                     {UnpackedSize + SizeBase, R1}
                             end,
            <<Data:Count/binary, Rest2/binary>> = Rest1,
            case unpack_varint(Count, Data) of
                WordValue when is_integer(WordValue) andalso Tag =:= tag_i ->
                    Shift = ?CHAR_BIT * (?sizeof_SWord - Count),
                    SignExtendedValue = (WordValue bsl Shift) bsr Shift,
                    if
                        %% This first clause is true at compile-time.
                        ?IS_SSMALL(SignExtendedValue) ->
                            {#tag{tag = Tag,
                                  word_value = SignExtendedValue,
                                  size = 0},
                             Rest2}
                    end;
                WordValue when is_integer(WordValue) andalso WordValue >= 0 ->
                    {#tag{tag = Tag,
                          word_value = WordValue,
                          size = 0},
                     Rest2};
                false ->
                    {#tag{tag = tag_o,
                          ptr_value = Data,
                          size = Count},
                     Rest2}
            end
    end.

decode_tag(0) -> tag_u;
decode_tag(1) -> tag_i;
decode_tag(2) -> tag_a;
decode_tag(3) -> tag_x;
decode_tag(4) -> tag_y;
decode_tag(5) -> tag_f;
decode_tag(6) -> tag_h;
decode_tag(7) -> tag_z.

unpack_varint(Size, Data) ->
    if
        Size =< ?sizeof_SWord -> do_unpack_varint(0, Size, Data, 0);
        true                  -> false
    end.

do_unpack_varint(I, Size, <<Byte:8/unsigned-integer, Rest/binary>>, Res)
  when I < Size ->
    Res1 = (Byte bsl (Size - I - 1) * ?CHAR_BIT) bor Res,
    do_unpack_varint(I + 1, Size, Rest, Res1);
do_unpack_varint(I, I = _Size, <<>> = _Rest, Res) ->
    Res.
