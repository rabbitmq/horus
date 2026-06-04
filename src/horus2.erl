%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus2).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("include/horus.hrl").
-include("src/horus_cover.hrl").
-include("src/horus_fun.hrl").
-include("src/horus_error.hrl").

-export([to_standalone_fun/1]).

to_standalone_fun(Fun) ->
    FunInfo = maps:from_list(erlang:fun_info(Fun)),
    #{module := _Module,
      name := _FunName,
      arity := Arity,
      env := Env} = FunInfo,
    Arity1 = Arity + length(Env),

    AbstractCode = horus_beam_utils:get_fun_abstract_code(Fun),
    logger:alert("Fun abstract code: ~p", [AbstractCode]),
    {'fun', Location, {clauses, Clauses}} = AbstractCode,
    Code = [{attribute,
             {1,1},
             file,
             {"/home/dumbbell/Documents/Dev/rabbitmq/horus/test/experiment.erl",
              1}},
            {attribute,{9,2},module,experiment},
            {attribute,0,export,[{test,0},{bitstring_flags_test,0}]},
            {attribute,
             {1,1},
             file,
             {"/usr/local/lib/erlang28/lib/eunit-2.10.3/include/eunit.hrl",
              1}},
            {attribute,
             {1,1},
             file,
             {"/usr/local/lib/erlang28/lib/stdlib-7.3/include/assert.hrl",
              1}},
            {attribute,
             {88,1},
             file,
             {"/usr/local/lib/erlang28/lib/eunit-2.10.3/include/eunit.hrl",
              88}},
            {attribute,
             {12,1},
             file,
             {"/home/dumbbell/Documents/Dev/rabbitmq/horus/test/experiment.erl",
              12}},
            {function,
             Location,
             bitstring_flags_test, Arity1, Clauses}],
    CompilerOptions = [binary,
                       warnings_as_errors,
                       return_errors,
                       return_warnings,
                       deterministic],
    compile:forms(Code, CompilerOptions).

%     Info = maps:from_list(erlang:fun_info(Fun)),
%     logger:alert("Info = ~p", [Info]),
%     #{module := Module,
%       name := _Name,
%       arity := _Arity} = Info,
%     Beam = get_beam(Module),
%     logger:alert("Beam = ~p", [beam_lib:all_chunks(Beam)]),
%     LambdaChunk = horus:get_and_decode_lambda_chunk(Module, Beam),
%     logger:alert("LambdaChunk = ~p", [LambdaChunk]),
%     LineChunk = horus:get_and_decode_line_chunk(Module, Beam),
%     logger:alert("LineChunk = ~p", [LineChunk]),
%     AbstractCode = get_abstract_code(Beam),
%     logger:alert("AbstractCode = ~p", [AbstractCode]),
%     logger:alert("Atoms = ~p", [beam_lib:chunks(Beam, [atoms])]),
%     % logger:alert("Literals = ~p", [beam_lib:chunks(Beam, [literals])]),
%     % logger:alert("Strings = ~p", [horus:get_and_decode_string_chunk(Module, Beam)]),
%     CompilerOptions = [from_abstr,
%                        'S',
%                        binary,
%                        return_errors,
%                        return_warnings,
%                        deterministic],
%     logger:alert("Asm = ~p", [compile:forms(AbstractCode, CompilerOptions)]),
%     ok.
%
% get_abstract_code(Beam) ->
%     Ret = beam_lib:chunks(Beam, [abstract_code]),
%     case Ret of
%         {ok, {_Module, [{abstract_code, {raw_abstract_v1, Code}}]}} ->
%             Code
%     end.
%
% get_beam(Module) ->
%     case code:get_object_code(Module) of
%         {_Module, Beam, _Filename} ->
%             Beam
%     end.
