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
-include("src/horus_fun.hrl").

-export([to_standalone_fun/1]).

-record(fun_extract, {module,
                      name,
                      arity,
                      fun_info = undefined,
                      abstract_code = undefined}).

-record(extraction, {'fun',
                     fun_info,
                     functions = #{}}).

to_standalone_fun(Fun) ->
    FunInfo = maps:from_list(erlang:fun_info(Fun)),
    InitialFunctions = #{Fun => undefined},
    Extraction = #extraction{'fun' = Fun,
                             fun_info = FunInfo,
                             functions = InitialFunctions},
    extract_missing_functions(Extraction).

extract_missing_functions(#extraction{functions = Functions} = Extraction) ->
    MissingFuns = maps:fold(
                    fun
                        (Reference, undefined, Acc)    -> [Reference | Acc];
                        (_Reference, _FunExtract, Acc) -> Acc
                    end, [], Functions),
    case MissingFuns of
        [] -> create_standanole_fun(Extraction);
        _  -> do_extract_missing_functions(MissingFuns, Extraction)
    end.

do_extract_missing_functions([MissingFun | Rest], Extraction) ->
    Extraction1 = extract_function(MissingFun, Extraction),
    do_extract_missing_functions(Rest, Extraction1);
do_extract_missing_functions([], Extraction) ->
    extract_missing_functions(Extraction).

extract_function(Fun, #extraction{'fun' = EntryPoint} = Extraction)
  when is_function(Fun) ->
    FunInfo = maps:from_list(erlang:fun_info(Fun)),
    #{module := Module,
      name := Name,
      arity := Arity,
      env := Env} = FunInfo,
    InternalName = case Fun =:= EntryPoint of
                       true  -> run;
                       false -> gen_function_name(Module, Name)
                   end,
    RealArity = Arity + length(Env),
    FunExtract = #fun_extract{module = Module,
                              name = InternalName,
                              arity = RealArity,
                              fun_info = FunInfo},
    do_extract_function(Fun, FunExtract, Extraction);
extract_function({Module, Name, Arity} = MFA, Extraction) ->
    InternalName = gen_function_name(Module, Name),
    FunExtract = #fun_extract{module =  Module,
                              name = InternalName,
                              arity = Arity},
    do_extract_function(MFA, FunExtract, Extraction).

do_extract_function(
  Reference,
  #fun_extract{module = ThisModule,
               name = InternalName,
               arity = RealArity} = FunExtract,
  Extraction) ->
    {ok, AbstractCode1} = horus_abscode_utils:get(Reference),

    %% Goals:
    %% 1. Is the expression allowed?
    %% 2. Find new calls
    PreCallback1 = fun
                       (#call{call = Call, args = CallArgs} = Expr,
                        _Vars,
                        #extraction{functions = Functions} = Extraction1) ->
                           CallReference = (
                             case Call of
                                 %% Local call.
                                 #atom{name = CalledName} ->
                                     CallArity = length(CallArgs),
                                     {ThisModule, CalledName, CallArity}
                             end),
                           Extraction2 = (
                             case Functions of
                                 #{CallReference := _} ->
                                     Extraction1;
                                 _ ->
                                     Functions1 = Functions#{
                                                    CallReference => undefined
                                                   },
                                     Extraction1#extraction{
                                       functions = Functions1
                                      }
                             end),
                           {continue, Expr, Extraction2};
                       (Expr, _Vars, Extraction1) ->
                           {continue, Expr, Extraction1}
                   end,

    %% Goals:
    %% 1. Add missing arguments for `fun()' taking arguments from their
    %%    environment.
    PostCallback1 = fun
                        (#clause{args = Args} = Expr, Vars, Extraction1) ->
                            UnboundVars1 = maps:fold(
                                             fun
                                                 (_Name, true, Acc) ->
                                                     Acc;
                                                (Name, false, Acc) ->
                                                     [Name | Acc]
                                             end, [], Vars),
                            UnboundVars2 = lists:sort(UnboundVars1),
                            ArgsFromEnv = [#var{location = 0,
                                                name = UnboundVar}
                                           || UnboundVar <- UnboundVars2],
                            Args1 = Args ++ ArgsFromEnv,
                            Expr1 = Expr#clause{args = Args1},
                            {continue, Expr1, Extraction1};
                       (Expr, _Vars, Extraction1) ->
                            {continue, Expr, Extraction1}
                    end,
    {ok, AbstractCode2, Extraction2} = horus_abscode_utils:fold(
                                         AbstractCode1,
                                         PreCallback1, PostCallback1,
                                         Extraction),

    %% Goals:
    %% 1. Transform a `fun()' into a regular function.
    PreCallback2 = fun
                       (#'fun'{location = Location,
                               code = #clauses{clauses = Clauses}},
                        _Vars,
                        Priv) ->
                           Expr1 = #function{location = Location,
                                             name = InternalName,
                                             arity = RealArity,
                                             clauses = Clauses},
                           {skip, Expr1, Priv};
                       (Expr, _Vars, Priv) ->
                           {continue, Expr, Priv}
                   end,
    {ok, AbstractCode3, _} = horus_abscode_utils:fold(
                               AbstractCode2,
                               PreCallback2, none,
                               undefined),

    FunExtract1 = FunExtract#fun_extract{abstract_code = AbstractCode3},
    #extraction{functions = Functions} = Extraction2,
    Functions1 = Functions#{Reference => FunExtract1},
    Extraction3 = Extraction2#extraction{functions = Functions1},
    Extraction3.

create_standanole_fun(
  #extraction{'fun' = Fun, functions = Functions} = Extraction) ->
    GeneratedModuleName = gen_module_name(Extraction),
    EntryPoint = maps:get(Fun, Functions),
    #fun_extract{name = EntryPointName,
                 arity = EntryPointArity,
                 fun_info = #{arity := Arity,
                              env := Env}} = EntryPoint,
    FunctionsRefs = lists:sort(maps:keys(Functions)),
    FunctionsAbstractCode = lists:foldl(
                              fun(Reference, Acc) ->
                                      #fun_extract{
                                         abstract_code = AbstractCode
                                        } = maps:get(Reference, Functions),
                                      Acc ++ AbstractCode
                              end, [], FunctionsRefs),
    AbstractCode = [#attribute{location = 0,
                               name = module,
                               value = GeneratedModuleName},
                    #attribute{location = 0,
                               name = export,
                               value = [{EntryPointName, EntryPointArity}]} |
                    FunctionsAbstractCode],
    logger:alert(
      "Generated module abstract code:~n~p~n",
      [AbstractCode]),
    logger:alert(
      "Generated module Erlang source code:~n~ts~n",
      [horus_abscode_utils:to_erlang_code(AbstractCode)]),

    StandaloneFun = #horus_fun{
                       module = GeneratedModuleName,
                       beam = AbstractCode,
                       arity = Arity,
                       literal_funs = [],
                       fun_name_mapping = #{{EntryPointName, EntryPointArity} => {a, b, 0}},
                       env = Env},

    {ok, StandaloneFun}.

-spec gen_module_name(Extraction) -> Module when
      Extraction :: #extraction{},
      Module :: module().

gen_module_name(#extraction{fun_info = Info, functions = Functions}) ->
    #{module := Module,
      name := Name} = Info,
    Checksum = erlang:phash2(Functions),
    InternalName = lists:flatten(
                     io_lib:format(
                       "horus__~s__~s__~b", [Module, Name, Checksum])),
    list_to_atom(InternalName).

-spec gen_function_name(Module, Name) -> Name when
      Module :: module(),
      Name :: atom().

gen_function_name(Module, Name) ->
    InternalName = lists:flatten(
                     io_lib:format(
                       "~s__~s", [Module, Name])),
    list_to_atom(InternalName).
