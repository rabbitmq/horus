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

-include("src/horus_abscode_utils.hrl").

-export([to_standalone_fun/1]).

to_standalone_fun(Fun) ->
    FunInfo = maps:from_list(erlang:fun_info(Fun)),
    #{module := Module,
      name := _FunName,
      arity := Arity,
      env := Env} = FunInfo,
    Arity1 = Arity + length(Env),

    %% Make a list of needed functions with:
    %% * origin: module:function/arity + filename + line number
    %% * abstract code
    %% * what they call (?)
    %%
    %% When preparing a particular function:
    %% * look for undefined variables
    %% * look for calls
    %% * look for allowed/denied expressions
    %% * patch calls: the arguments of the call should be enough to know
    %%   deterministically what the new embedded call will be, right?

    {ok, AbstractCode} = horus_abscode_utils:get(Fun),
    logger:alert("Fun abstract code: ~p", [AbstractCode]),

    PreCallback = fun
                      (#call{call = Call, args = CallArgs} = Expr, _Vars, #{calls := Calls} = Priv1) ->
                          case Call of
                              #atom{name = Name} ->
                                  CallArity = length(CallArgs),
                                  Calls1 = Calls#{{Module, Name, CallArity} => true},
                                  Priv2 = Priv1#{calls => Calls1},
                                  {continue, Expr, Priv2}
                          end;
                      (Expr, _Vars, Priv1) ->
                          % logger:alert("[pre] Expr = ~p~nVars = ~p", [Expr, Vars]),
                          {continue, Expr, Priv1}
                  end,
    PostCallback = fun
                       (#clause{args = Args} = Expr, Vars, Priv1) ->
                           UnboundVars1 = maps:fold(
                                            fun
                                                (_Name, true, Acc) ->
                                                    Acc;
                                                (Name, false, Acc) ->
                                                    [Name | Acc]
                                            end, [], Vars),
                           UnboundVars2 = lists:sort(UnboundVars1),
                           ArgsFromEnv = [#var{location = {0, 0},
                                               name = UnboundVar}
                                          || UnboundVar <- UnboundVars2],
                           Args1 = Args ++ ArgsFromEnv,
                           Expr1 = Expr#clause{args = Args1},
                           {continue, Expr1, Priv1};
                       (Expr, _Vars, Priv1) ->
                           {continue, Expr, Priv1}
                   end,
    Priv = #{module => Module,
             calls => #{}},
    Ret = horus_abscode_utils:fold(
            AbstractCode,
            PreCallback, PostCallback,
            Priv),
    logger:alert("Ret = ~p", [Ret]),
    {ok, AbstractCode1, NewPriv} = Ret,

    #{calls := Calls} = NewPriv,
    [{M, F, A} | _] = maps:keys(Calls),
    {ok, AC} = horus_abscode_utils:get(M, F, A),
    logger:alert("~s:~s/~b = ~p", [M, F, A, AC]),

    {'fun', Location, {clauses, Clauses}} = AbstractCode1,
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
    logger:alert(
      "Generated module abstract code:~n  ~p~nAs Erlang source code:~n~ts",
      [Code, horus_abscode_utils:to_erlang_code(Code)]),

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
