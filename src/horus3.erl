%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus3).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("src/horus_fun.hrl").

-export([to_standalone_fun/1,
         to_standalone_fun/2]).

-record(fun_extract, {module,
                      name,
                      arity,
                      fun_info = undefined,
                      core_erlang = undefined}).

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
    % logger:alert("Env = ~p~nArity = ~b -> ~b", [Env, Arity, RealArity]),
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
               name = _InternalName,
               arity = RealArity} = FunExtract,
  Extraction) ->
    {ok, CoreErlang1} = horus_cerl_utils:get(Reference),
    % ?LOG_ALERT("Horus: function ~0p Core Erlang:~n~p", [Reference, CoreErlang1]),

    %% Goals:
    %% 1. Is the expression allowed?
    %% 2. Find new calls
    PreCallback1 = fun(Node, Fold, {Vars, FunDepths, #extraction{functions = Functions} = Extraction1} = Acc) ->
                           case cerl:type(Node) of
                               letrec ->
                                   Definitions = cerl:letrec_defs(Node),
                                   Functions1 = lists:foldl(
                                                  fun({Var, _Fun}, Fs) ->
                                                          {N, A} = cerl:var_name(Var),
                                                          Fs#{{ThisModule, N, A} => comprehension}
                                                  end, Functions, Definitions),
                                   Extraction2 = Extraction1#extraction{functions = Functions1},
                                   {in, {Vars, FunDepths, Extraction2}};
                               apply ->
                                   Op = cerl:apply_op(Node),
                                   case cerl:is_c_var(Op) of
                                       true ->
                                           case cerl:var_name(Op) of
                                               {Name, Arity} ->
                                                   CallRef = {ThisModule, Name, Arity},
                                                   Extraction2 = case Functions of
                                                                     #{CallRef := _} ->
                                                                         Extraction1;
                                                                     _ ->
                                                                         Functions1 = Functions#{
                                                                                        CallRef => undefined
                                                                                       },
                                                                         Extraction1#extraction{
                                                                           functions = Functions1
                                                                          }
                                                                 end,
                                                   #extraction{functions = Functions2} = Extraction2,
                                                   Node1 = case Functions2 of
                                                               #{CallRef := comprehension} ->
                                                                   Node;
                                                               _ ->
                                                                   LocalFunName = gen_function_name(
                                                                                    ThisModule,
                                                                                    Name),
                                                                   Op1 = cerl:update_c_var(Op, {LocalFunName, Arity}),
                                                                   cerl:update_c_apply(
                                                                     Node,
                                                                     Op1,
                                                                     cerl:apply_args(Node))
                                                           end,
                                                   {in, Node1, {Vars, FunDepths, Extraction2}};
                                               _ ->
                                                   {in, Acc}
                                           end;
                                       false ->
                                           {in, Acc}
                                   end;
                               'fun' ->
                                   Depth = horus_cerl_utils:get_depth(Fold),
                                   PrevFuns = maps:get(Depth, Vars, []),
                                   VarsAtDepth = [#{} | PrevFuns],
                                   Vars1 = Vars#{Depth => VarsAtDepth},
                                   FunDepths1 = [Depth | FunDepths],
                                   {in, {Vars1, FunDepths1, Extraction1}};
                               var ->
                                   VarName = cerl:var_name(Node),
                                   if
                                       is_atom(VarName) orelse is_integer(VarName) ->
                                           Matching = horus_cerl_utils:get_matching(Fold),
                                           Depth = hd(FunDepths),
                                           [CurrentFun | PrevFuns] = maps:get(Depth, Vars),
                                           Vars1 = case CurrentFun of
                                                       #{VarName := _} ->
                                                           Vars;
                                                       _  ->
                                                           UpperDepths = tl(FunDepths),
                                                           Defd = lists:any(
                                                                    fun(UD) ->
                                                                            [F | _] = maps:get(UD, Vars),
                                                                            maps:get(VarName, F, false)
                                                                    end, UpperDepths),
                                                           case Defd of
                                                               true ->
                                                                   Vars;
                                                               false ->
                                                                   CurrentFun1 = CurrentFun#{VarName => Matching},
                                                                   VarsAtDepth = [CurrentFun1 | PrevFuns],
                                                                   Vars#{Depth => VarsAtDepth}
                                                           end
                                                   end,
                                           {in, {Vars1, FunDepths, Extraction1}};
                                       true ->
                                           {in, Acc}
                                   end;
                               _ ->
                                   {in, Acc}
                           end
                   end,

    %% Goals:
    %% 1. Add missing arguments for `fun()' taking arguments from their
    %%    environment.
    PostCallback1 = fun(Node, Fold, {Vars, FunDepths, Extraction1} = Acc) ->
                            case cerl:type(Node) of
                                'fun' ->
                                    case horus_cerl_utils:get_depth(Fold) of
                                        0 ->
                                            % FunName = {InternalName, RealArity},
                                            % Ann = cerl:get_ann(Node),
                                            % Ann1 = lists:keydelete(function, 1, Ann),
                                            % Ann2 = lists:keydelete(id, 1, Ann1),
                                            % Ann3 = [{function, FunName} | Ann2],
                                            UndefVars1 = maps:map(
                                                           fun(_Depth, VarsAtDepth) ->
                                                                   VarsAtDepth1 = [begin
                                                                                       V1 = maps:fold(
                                                                                              fun
                                                                                                  (_VarName, true, Acc1) ->
                                                                                                      Acc1;
                                                                                                  (VarName, false, Acc1) ->
                                                                                                      [VarName | Acc1]
                                                                                              end, [], V),
                                                                                       V2 = lists:sort(V1),
                                                                                       V2
                                                                                   end || V <- VarsAtDepth],
                                                                   VarsAtDepth2 = lists:flatten(VarsAtDepth1),
                                                                   VarsAtDepth2
                                                           end, Vars),
                                            Ks = lists:reverse(lists:sort(maps:keys(UndefVars1))),
                                            UndefVars2 = [maps:get(K, UndefVars1) || K <- Ks],
                                            UndefVars3 = lists:flatten(UndefVars2),
                                            % UndefVars1 = [VarName || {VarName, false} <- Vars],
                                            % UndefVars2 = lists:reverse(UndefVars1),
                                            % UndefVars3 = lists:sort(UndefVars2),
                                            % UndefVars3 = lists:sort(
                                            %                fun
                                            %                    (A, B) when is_atom(A) andalso is_atom(B) ->
                                            %                        true;
                                            %                    (A, B) when is_integer(A) andalso is_integer(B) ->
                                            %                        A < B;
                                            %                    (A, B) ->
                                            %                        is_atom(A) andalso is_integer(B)
                                            %                end, UndefVars2),
                                            % io:format(standard_error, "----- VARS: ~0p -> ~0p~n", [Vars, UndefVars3]),
                                            UndefVars4 = [cerl:c_var(VarName)
                                                          || VarName <- UndefVars3],
                                            Args = cerl:fun_vars(Node),
                                            Args1 = Args ++ UndefVars4,
                                            ?assertEqual(RealArity, length(Args1)),
                                            % Node1 = cerl:set_ann(Node, Ann3),
                                            Node1 = Node,
                                            Node2 = cerl:update_c_fun(
                                                      Node1,
                                                      Args1,
                                                      cerl:fun_body(Node)),
                                            {ok, Node2, Acc};
                                        _ ->
                                            FunDepths1 = tl(FunDepths),
                                            {ok, Node, {Vars, FunDepths1, Extraction1}}
                                    end;
                                _ ->
                                    {ok, Node, Acc}
                            end
                    end,
    {ok, CoreErlang2, {_Vars, _FunDepths, Extraction2}} = horus_cerl_utils:fold(
                                                            CoreErlang1,
                                                            PreCallback1, PostCallback1,
                                                            {#{}, [], Extraction}),

    FunExtract1 = FunExtract#fun_extract{core_erlang = CoreErlang2},
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
    FunctionsCoreErlang = lists:foldr(
                            fun(Reference, Acc) ->
                                    case Functions of
                                        #{Reference := #fun_extract{
                                                          name = InternalName,
                                                          arity = RealArity,
                                                          core_erlang = CoreErlang
                                                         }} ->
                                            % Ann = cerl:get_ann(CoreErlang),
                                            % {function, FunName} = lists:keyfind(function, 1, Ann),
                                            FunName = {InternalName, RealArity},
                                            FunRef = {cerl:c_var(FunName), CoreErlang},
                                            [FunRef | Acc];
                                        #{Reference := comprehension} ->
                                            Acc
                                    end
                            end, [], FunctionsRefs),
    Exports = [cerl:c_var({EntryPointName, EntryPointArity})],
    ModuleCoreErlang = cerl:c_module(
                         cerl:c_atom(GeneratedModuleName),
                         Exports,
                         FunctionsCoreErlang),
    % ?LOG_ALERT(
    %    "Generated module Core Erlang:~n~p~n",
    %    [ModuleCoreErlang]),
    horus_cerl_utils:format(ModuleCoreErlang),

    FunNameMapping = gen_fun_name_mapping(Functions),
    StandaloneFun = #horus_fun{
                       module = GeneratedModuleName,
                       beam = ModuleCoreErlang,
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
    Acc#{{Name, Arity} => {M, F, A}};
gen_fun_name_mapping1(
  _MFA,
  comprehension,
  Acc) ->
    Acc.
