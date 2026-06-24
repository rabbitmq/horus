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

-export([to_standalone_fun/1,
         to_standalone_fun/2]).

-record(fun_extract, {module,
                      name,
                      arity,
                      fun_info = undefined,
                      abstract_code = undefined}).

-record(extraction, {'fun',
                     fun_info,
                     functions = #{}}).

-define(SF_ENTRYPOINT, run).

to_standalone_fun(Fun) ->
    to_standalone_fun(Fun, #{}).

to_standalone_fun(Fun, _Options) ->
    FunInfo = horus_erlfun_utils:info(Fun),
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
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module,
      name := Name,
      arity := Arity,
      env := Env} = FunInfo,
    InternalName = case Fun =:= EntryPoint of
                       true  -> ?SF_ENTRYPOINT;
                       false -> gen_function_name(Module, Name)
                   end,
    RealArity = Arity + length(Env),
    logger:alert("Env = ~p~nArity = ~b -> ~b", [Env, Arity, RealArity]),
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
    logger:alert("Abstract code (~s:~s/~b): ~p", [ThisModule, InternalName, RealArity, AbstractCode1]),
    % throw(pouet),

    %% Goals:
    %% 1. Is the expression allowed?
    %% 2. Find new calls
    PreCallback1 = fun
                       (#call{call = Call, args = CallArgs} = Expr,
                        _Fold,
                        #extraction{functions = Functions} = Extraction1) ->
                           CallReference = (
                             case Call of
                                 %% Local call.
                                 #atom{name = CalledName} ->
                                     CallArity = length(CallArgs),
                                     {ThisModule, CalledName, CallArity};
                                 #remote{module = #atom{name = CalledModule},
                                         function = #atom{name = CalledName}} ->
                                     CallArity = length(CallArgs),
                                     {CalledModule, CalledName, CallArity}
                             end),
                           Extraction2 = (
                             case Functions of
                                 #{CallReference := _} ->
                                     Extraction1;
                                 _ when is_tuple(CallReference) andalso
                                        (element(1, CallReference) =:= maps orelse
                                         element(1, CallReference) =:= lists orelse
                                         element(1, CallReference) =:= proplists orelse
                                         element(1, CallReference) =:= horus2 orelse
                                         element(1, CallReference) =:= helpers) ->
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
                       (Expr, _Fold, Extraction1) ->
                           {continue, Expr, Extraction1}
                   end,

    %% Goals:
    %% 1. Add missing arguments for `fun()' taking arguments from their
    %%    environment.
    PostCallback1 = fun
                        (#'fun'{location = Location,
                                code = #clauses{clauses = Clauses}} = Expr,
                         Fold,
                         Extraction1) ->
                            case horus_abscode_utils:get_expr_depth(Fold) of
                                1 ->
                                    Expr1 = #function{location = Location,
                                                      name = InternalName,
                                                      arity = RealArity,
                                                      clauses = Clauses},
                                    {continue, Expr1, Extraction1};
                                _ ->
                                    {continue, Expr, Extraction1}
                            end;
                        (#function{} = Expr, _Fold, Extraction1) ->
                            Expr1 = Expr#function{name = InternalName},
                            {continue, Expr1, Extraction1};
                        (#call{call = Call} = Expr, _Fold, Extraction1) ->
                            Expr1 = case Call of
                                        #atom{name = LocalFunName} ->
                                            LocalFunName1 = gen_function_name(
                                                              ThisModule,
                                                              LocalFunName),
                                            Call1 = Call#atom{name = LocalFunName1},
                                            Expr#call{call = Call1};
                                        #remote{module = #atom{name = CalledModule}}
                                          when CalledModule =:= maps orelse
                                               CalledModule =:= lists orelse
                                               CalledModule =:= proplists orelse
                                               CalledModule =:= horus2 orelse
                                               CalledModule =:= helpers ->
                                            Expr;
                                        #remote{location = Location,
                                                module = #atom{name = CalledModule},
                                                function = #atom{name = CalledFunName}} ->
                                            LocalFunName1 = gen_function_name(
                                                              CalledModule,
                                                              CalledFunName),
                                            Call1 = #atom{location = Location,
                                                          name = LocalFunName1},
                                            Expr#call{call = Call1}
                                    end,
                            {continue, Expr1, Extraction1};
                       (Expr, _Fold, Extraction1) ->
                            {continue, Expr, Extraction1}
                    end,
    {ok, AbstractCode2, Extraction2} = horus_abscode_utils:fold(
                                         AbstractCode1,
                                         Reference,
                                         PreCallback1, PostCallback1,
                                         Extraction),

    FunExtract1 = FunExtract#fun_extract{abstract_code = AbstractCode2},
    #extraction{functions = Functions} = Extraction2,
    Functions1 = Functions#{Reference => FunExtract1},
    Extraction3 = Extraction2#extraction{functions = Functions1},
    Extraction3.

% handle_unbound_vars(#clause{args = Args, body = Body} = Expr, Fold) ->
%     Vars = horus_abscode_utils:get_vars(Fold),
%     PredefinedVars = horus_abscode_utils:get_scope(Fold),
%     UnboundVars1 = maps:fold(
%                      fun
%                          (_Name, true, Acc) -> Acc;
%                          (Name, false, Acc) -> [Name | Acc]
%                      end, [], Vars),
%     UnboundVars2 = UnboundVars1 -- ['ApplyTo', 'List'],
%     UnboundVars3 = lists:sort(UnboundVars2),
%     ArgsFromEnv = [#var{location = 0,
%                         name = UnboundVar}
%                    || UnboundVar <- UnboundVars3],
%     Args1 = Args ++ ArgsFromEnv,
%     Body1 = PredefinedVars ++ Body,
%     Expr1 = Expr#clause{args = Args1, body = Body1},
%     Expr1.

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

    FunNameMapping = gen_fun_name_mapping(Functions),
    StandaloneFun = #horus_fun{
                       module = GeneratedModuleName,
                       beam = AbstractCode,
                       arity = Arity,
                       literal_funs = [],
                       fun_name_mapping = FunNameMapping,
                       env = Env},

    % {ok, StandaloneFun}.
    StandaloneFun.

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

gen_fun_name_mapping(Functions) ->
    FunNameMapping = maps:fold(fun gen_fun_name_mapping1/3, #{}, Functions),
    FunNameMapping.

gen_fun_name_mapping1(
  MFA,
  #fun_extract{name = Name, arity = Arity},
  Acc)
  when is_tuple(MFA) ->
    Acc#{{Name, Arity} => MFA};
gen_fun_name_mapping1(
  Fun,
  #fun_extract{name = Name, arity = Arity, fun_info = FunInfo},
  Acc)
  when is_function(Fun) ->
    #{module := M,
      name := F,
      arity := A} = FunInfo,
    Acc#{{Name, Arity} => {M, F, A}}.
