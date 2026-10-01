%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus4).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("src/horus_error.hrl").
-include("src/horus_fun.hrl").

-export([% to_standalone_fun/1,
         to_standalone_fun/2 %,
         % to_fun/1,
         % exec/2
        ]).

% -ifdef(TEST).
% -export([compile/1]).
% -endif.

-record(horus_fs, {reference,
                   info,
                   internal_name,
                   internal_arity,
                   core_erlang}).

-record(horus_gs, {entrypoint :: fun(),
                   options = #{} :: horus4:options(),

                   functions :: #{horus4:fun_ref() => #horus_fs{} | comprehension | undefined | {undefined, local | external}},
                   calls = #{} :: calls_map(),

                   errors = []
                  }).

-type options() :: #{% should_process_function =>
                     % should_process_function_fun(),
                     %
                     % is_standalone_fun_still_needed =>
                     % is_standalone_fun_still_needed_fun(),

                     compile_options => list()}.

-type fun_ref() :: fun() | {module(), atom(), arity()}.

-type horus_fun() :: #horus_fun{} | fun().
%% The result of an extraction, as returned by {@link to_standalone_fun/2}.
%%
%% It can be stored, passed between processes and Erlang nodes. To execute the
%% extracted function, simply call {@link exec/2} which works like {@link
%% erlang:apply/2}.

-type calls_map() :: #{mfa() => true}.
%% The `calls' map, used to calls made by the extracted function and all
%% the functions it calls, included functions passed as argument or in its
%% environment.

-export_type([options/0,
              fun_ref/0]).

-define(SF_ENTRYPOINT, run).

to_standalone_fun(Entrypoint, Options) when is_function(Entrypoint) ->
    ?LOG_DEBUG(
       "Horus: starting extraction for entrypoint ~0p",
       [Entrypoint],
       #{domain => [horus, extract]}),
    {StandaloneFun, _GS} = to_standalone_fun1(Entrypoint, Options),
    StandaloneFun.

to_standalone_fun1(Entrypoint, Options) ->
    Info = horus_erlfun_utils:info(Entrypoint),
    #{module := Module,
      name := Name,
      arity := Arity} = Info,
    GS = #horus_gs{entrypoint = Entrypoint,
                   options = Options,

                   functions = #{Entrypoint => undefined}
                  },
    TmpFS = #horus_fs{info = Info},
    {ShouldProcess, GS1} = should_process_function(Module, Name, Arity, TmpFS, GS),
    case ShouldProcess of
        true  -> extract_next_unprocessed_function(GS1);
        false -> {Entrypoint, GS}
    end.

extract_next_unprocessed_function(#horus_gs{} = GS) ->
    case get_next_unprocessed_function(GS) of
        Fun when is_function(Fun) ->
            extract_fun(Fun, GS);
        MFA when is_tuple(MFA) ->
            extract_mfa(MFA, GS);
        none ->
            create_standalone_fun(GS)
    end.

get_next_unprocessed_function(#horus_gs{functions = Functions}) ->
    Iterator = maps:iterator(Functions, ordered),
    get_next_unprocessed_function1(Iterator).

get_next_unprocessed_function1(Iterator) ->
    case maps:next(Iterator) of
        {Function, undefined, _} ->
            Function;
        {Function, {undefined, _}, _} ->
            Function;
        {_Function, _FS, NextIterator} ->
            get_next_unprocessed_function1(NextIterator);
        none ->
            none
    end.

extract_fun(
  Fun,
  #horus_gs{functions = Functions} = GS) when is_function(Fun) ->
    ?assertEqual(undefined, maps:get(Fun, Functions)),
    Info = horus_erlfun_utils:info(Fun),
    FS = #horus_fs{reference = Fun,
                   info = Info},
    do_extract(FS, GS).

extract_mfa(
  {Module, Name, Arity} = MFA,
  #horus_gs{functions = Functions} = GS) ->
    Undef = maps:get(MFA, Functions),
    ?assertMatch(_ when Undef =:= {undefined, local} orelse Undef =:= {undefined, external}, Undef),
    {undefined, Type} = Undef,
    Info = #{module => Module,
             name => Name,
             arity => Arity,
             type => Type,
             env => []},
    FS = #horus_fs{reference = MFA,
                   info = Info},
    do_extract(FS, GS).

do_extract(#horus_fs{info = #{module := Module,
                              name := Name,
                              arity := Arity}} = FS, GS) ->
    case can_extract(FS, GS) of
        true ->
            do_extract1(FS, GS);
        false ->
            throw(?horus_error(
                     call_to_unexported_function,
                     #{mfa => {Module, Name, Arity}}))
    end.

%% FIXME: fun() vs MFS, local vs. external.
can_extract(
  #horus_fs{info = #{type := local}},
  _GS) ->
    true;
can_extract(
  #horus_fs{info = #{type := external,
                     module := Module,
                     name := Name,
                     arity := Arity}},
  GS) ->
    can_extract_external(Module, Name, Arity, GS).

can_extract_external(Module, Name, Arity, _GS) ->
    _ = try
            Module:module_info()
        catch
            _:_ ->
                ok
        end,
    %% TODO: Check an option to extract even non-exported functions.
    erlang:function_exported(Module, Name, Arity).

do_extract1(
  #horus_fs{reference = Reference,
            info = #{module := Module,
                     name := Name,
                     arity := Arity,
                     env := Env}} = FS,
  #horus_gs{entrypoint = Entrypoint} = GS) ->
    ?LOG_DEBUG(
       "Horus: extracting function ~s:~s/~b",
       [Module, Name, Arity],
       #{domain => [horus, extract]}),
    case horus_cerl:get(Reference) of
        {ok, CErl} ->
            InternalName = case Reference =:= Entrypoint of
                               true  -> ?SF_ENTRYPOINT;
                               false -> gen_function_name(Module, Name)
                           end,
            InternalArity = case Module of
                                erl_eval -> Arity;
                                _        -> Arity + length(Env)
                            end,
            FS1 = FS#horus_fs{internal_name = InternalName,
                              internal_arity = InternalArity,
                              core_erlang = CErl},
            % Functions1 = Functions#{Reference => FS1},
            % GS1 = GS#horus_gs{functions = Functions1},
            do_extract2(FS1, GS);
        {error, _} = Error ->
            % XXX
            throw(Error)
    end.

-record(priv, {fs,
               gs,
               vars,
               fun_depths}).

do_extract2(#horus_fs{reference = Reference, core_erlang = CErl} = FS, GS) ->
    PreCallback = fun extract_pre_callback/3,
    PostCallback = fun extract_post_callback/3,
    InitialVars = #{},
    FunDepths = [],
    Priv = #priv{fs = FS,
                 gs = GS,
                 vars = InitialVars,
                 fun_depths = FunDepths},
    case horus_cerl_fold:fold(CErl, PreCallback, PostCallback, Priv) of
        {ok, CErl1, #priv{gs = #horus_gs{functions = Functions} = GS1}} ->
            FS1 = FS#horus_fs{core_erlang = CErl1},
            Functions1 = Functions#{Reference => FS1},
            GS2 = GS1#horus_gs{functions = Functions1},
            extract_next_unprocessed_function(GS2);
        {error, _} = Error ->
            % XXX
            throw(Error)
    end.

extract_pre_callback(CNode, Fold, #priv{gs = GS} = Priv) ->
    GS1 = ensure_cerl_node_is_permitted(CNode, GS),
    Priv1 = Priv#priv{gs = GS1},
    CNodeType = cerl:type(CNode),
    extract_pre_callback1(CNodeType, CNode, Fold, Priv1).

extract_pre_callback1(
  letrec, CNode, _Fold, #priv{fs = FS, gs = GS} = Priv) ->
    #horus_fs{info = #{module := ThisModule}} = FS,
    #horus_gs{functions = Functions} = GS,

    Definitions = cerl:letrec_defs(CNode),
    Functions1 = lists:foldl(
                   fun({Var, _Fun}, Acc) ->
                           {Name, Arity} = cerl:var_name(Var),
                           CallRef = {ThisModule, Name, Arity},
                           Acc#{CallRef => comprehension}
                   end, Functions, Definitions),
    GS1 = GS#horus_gs{functions = Functions1},
    Priv1 = Priv#priv{gs = GS1},
    {in, Priv1};
extract_pre_callback1(
  apply, CNode, _Fold, #priv{fs = FS, gs = GS} = Priv) ->
    #horus_fs{info = #{module := ThisModule}} = FS,

    Op = cerl:apply_op(CNode),
    case cerl:is_c_var(Op) of
        true ->
            case cerl:var_name(Op) of
                {Name, Arity} ->
                    io:format("APPLY ~p:~p~n", [Name, Arity]),
                    case Name of
                        %% Distinguer les "goto" qu’on ne doit pas suivre.
                        'recv$^0' ->
                            io:format("~p~n", [FS]),
                            throw(stop);
                        _ ->
                            ok
                    end,
                    GS1 = record_call(ThisModule, Name, Arity, local, FS, GS),
                    Priv1 = Priv#priv{gs = GS1},

                    #horus_gs{functions = Functions1} = GS1,
                    CallRef = {ThisModule, Name, Arity},
                    CNode1 = case Functions1 of
                                 #{CallRef := comprehension} ->
                                     CNode;
                                 #{CallRef := _} ->
                                     InternalName = gen_function_name(
                                                      ThisModule,
                                                      Name),
                                     Op1 = cerl:update_c_var(
                                             Op, {InternalName, Arity}),
                                     cerl:update_c_apply(
                                       CNode,
                                       Op1,
                                       cerl:apply_args(CNode));
                                 _ ->
                                     CNode
                             end,
                    {in, CNode1, Priv1};
                _ ->
                    {in, Priv}
            end;
        false ->
            {in, Priv}
    end;
extract_pre_callback1(
  call, CNode, _Fold, #priv{fs = FS, gs = GS} = Priv) ->
    #horus_fs{info = #{module := ThisModule}} = FS,

    ModuleNode = cerl:call_module(CNode),
    NameNode = cerl:call_name(CNode),
    Arity = cerl:call_arity(CNode),
    IsDynamic = not (cerl:is_literal(ModuleNode) andalso
                     cerl:is_literal(NameNode)),
    case IsDynamic of
        false ->
            Module = cerl:concrete(ModuleNode),
            Name = cerl:concrete(NameNode),
            case can_extract_external(Module, Name, Arity, GS) of
                true ->
                    ok;
                false when Module =:= ThisModule ->
                    ok;
                false ->
                    throw(?horus_error(
                             call_to_unexported_function,
                             #{mfa => {Module, Name, Arity}}))
            end,
            GS1 = record_call(Module, Name, Arity, external, FS, GS),
            Priv1 = Priv#priv{gs = GS1},

            #horus_gs{functions = Functions1} = GS1,
            CallRef = {ThisModule, Name, Arity},
            case Functions1 of
                #{CallRef := _} ->
                    InternalName = gen_function_name(
                                     Module,
                                     Name),
                    CNode1 = cerl:ann_c_apply(
                               cerl:get_ann(CNode),
                               cerl:ann_c_var(
                                 cerl:get_ann(CNode),
                                 {InternalName, Arity}),
                               cerl:call_args(CNode)),
                    {in, CNode1, Priv1};
                _ ->
                    {in, Priv1}
            end;
        true ->
            Module = case cerl:is_literal(ModuleNode) of
                         true  -> cerl:concrete(ModuleNode);
                         false -> '$dynamic'
                     end,
            Name = case cerl:is_literal(NameNode) of
                       true  -> cerl:concrete(NameNode);
                       false -> '$dynamic'
                   end,
            {_ShouldProcess, GS1} = should_process_function(
                                       Module, Name, Arity,
                                       FS, GS),
            Priv1 = Priv#priv{gs = GS1},
            {in, Priv1}
    end;
extract_pre_callback1(
  'fun', _CNode, Fold, #priv{vars = Vars, fun_depths = FunDepths} = Priv) ->
    Depth = horus_cerl_fold:get_depth(Fold),
    PrevFuns = maps:get(Depth, Vars, []),
    VarsAtDepth = [#{} | PrevFuns],
    Vars1 = Vars#{Depth => VarsAtDepth},
    FunDepths1 = [Depth | FunDepths],
    Priv1 = Priv#priv{vars = Vars1, fun_depths = FunDepths1},
    {in, Priv1};
extract_pre_callback1(
  var, CNode, Fold, #priv{vars = Vars, fun_depths = FunDepths} = Priv) ->
    VarName = cerl:var_name(CNode),
    if
        is_atom(VarName) orelse is_integer(VarName) ->
            Matching = horus_cerl_fold:get_matching(Fold),
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
            Priv1 = Priv#priv{vars = Vars1},
            {in, Priv1};
        true ->
            {in, Priv}
    end;
extract_pre_callback1(_CNodeType, _CNode, _Fold, Priv) ->
    {in, Priv}.

extract_post_callback(CNode, Fold, Priv) ->
    CNodeType = cerl:type(CNode),
    extract_post_callback1(CNodeType, CNode, Fold, Priv).

extract_post_callback1(
  'fun', CNode, Fold,
  #priv{fs = FS, vars = Vars, fun_depths = FunDepths} = Priv) ->
    #horus_fs{internal_arity = InternalArity} = FS,

    case horus_cerl_fold:get_depth(Fold) of
        0 ->
            UndefVars1 = maps:map(
                           fun(_Depth, VarsAtDepth) ->
                                   VarsAtDepth1 = (
                                     [begin
                                          V1 = maps:fold(
                                                 fun
                                                     (_VarName, true, Acc1) ->
                                                         Acc1;
                                                     (VarName, false, Acc1) ->
                                                         [VarName | Acc1]
                                                 end, [], V),
                                          V2 = lists:sort(V1),
                                          V2
                                      end || V <- VarsAtDepth]),
                                   VarsAtDepth2 = lists:flatten(VarsAtDepth1),
                                   VarsAtDepth2
                           end, Vars),
            Ks = lists:reverse(lists:sort(maps:keys(UndefVars1))),
            UndefVars2 = [maps:get(K, UndefVars1) || K <- Ks],
            UndefVars3 = lists:flatten(UndefVars2),
            UndefVars4 = [cerl:c_var(VarName) || VarName <- UndefVars3],
            Args = cerl:fun_vars(CNode),
            Args1 = Args ++ UndefVars4,
            % io:format("FS = ~p~nInternalArity = ~b~nArgs1 = ~0p~n", [FS, InternalArity, Args1]),
            ?assertEqual(InternalArity, length(Args1)),
            CNode1 = CNode,
            CNode2 = cerl:update_c_fun(
                       CNode1,
                       Args1,
                       cerl:fun_body(CNode)),
            {ok, CNode2, Priv};
        _ ->
            FunDepths1 = tl(FunDepths),
            Priv1 = Priv#priv{fun_depths = FunDepths1},
            {ok, CNode, Priv1}
    end;
extract_post_callback1(_CNodeType, _CNode, _Fold, Priv) ->
    {ok, Priv}.

create_standalone_fun(
  #horus_gs{entrypoint = Entrypoint, functions = Functions} = GS) ->
    io:format("~0p: GS = ~p~n", [Entrypoint, GS]),
    EntrypointFS = maps:get(Entrypoint, Functions),
    {Env, GS1} = to_standalone_env(EntrypointFS, GS),
    case is_standalone_fun_still_needed(GS1) of
        true ->
            process_errors(GS1),

            GeneratedModuleName = gen_module_name(GS1),
            #horus_fs{info = #{module := Module,
                               arity := Arity},
                      internal_name = EntrypointName,
                      internal_arity = EntrypointArity
                     } = EntrypointFS,
            FunctionsRefs = lists:sort(maps:keys(Functions)),
            FunctionsCoreErlang = lists:foldr(
                                    fun(Reference, Acc) ->
                                            case Functions of
                                                #{Reference := #horus_fs{
                                                                  internal_name = InternalName,
                                                                  internal_arity = InternalArity,
                                                                  core_erlang = CoreErlang
                                                                 }} ->
                                                    % Ann = cerl:get_ann(CoreErlang),
                                                    % {function, FunName} = lists:keyfind(function, 1, Ann),
                                                    FunName = {InternalName, InternalArity},
                                                    FunRef = {cerl:c_var(FunName), CoreErlang},
                                                    [FunRef | Acc];
                                                #{Reference := comprehension} ->
                                                    Acc
                                            end
                                    end, [], FunctionsRefs),
            % io:format(standard_error, "CORE ERLANG:~n~p~n", [FunctionsCoreErlang]),
            MIExports = [cerl:c_var({module_info, 0}),
                         cerl:c_var({module_info, 1})],
            MICoreErlang = [{cerl:c_var({module_info, 0}),
                             cerl:ann_c_fun(
                               [{function, {module_info, 0}}],
                               [],
                               cerl:c_call(
                                 cerl:abstract(erlang),
                                 cerl:abstract(get_module_info),
                                 [cerl:abstract(GeneratedModuleName)]))},
                            {cerl:c_var({module_info, 1}),
                             cerl:ann_c_fun(
                               [{function, {module_info, 0}}],
                               [cerl:c_var(0)],
                               cerl:c_call(
                                 cerl:abstract(erlang),
                                 cerl:abstract(get_module_info),
                                 [cerl:abstract(GeneratedModuleName),
                                  cerl:c_var(0)]))}],

            Exports = [cerl:c_var({EntrypointName, EntrypointArity})],
            ModuleCoreErlang = cerl:c_module(
                                 cerl:c_atom(GeneratedModuleName),
                                 Exports ++ MIExports,
                                 FunctionsCoreErlang ++ MICoreErlang),
            % ?LOG_ALERT(
            %    "Generated module Core Erlang:~n~p~n",
            %    [ModuleCoreErlang]),
            % horus_cerl:format(ModuleCoreErlang),

            FunNameMapping = gen_fun_name_mapping(Functions),
            Env1 = case Module of
                       erl_eval -> [];
                       _        -> Env
                   end,
            StandaloneFun = #horus_fun{
                               module = GeneratedModuleName,
                               beam = ModuleCoreErlang,
                               arity = Arity,
                               literal_funs = [],
                               fun_name_mapping = FunNameMapping,
                               env = Env1},

            {StandaloneFun, GS1};
        false ->
            {Entrypoint, GS}
    end.

-spec gen_module_name(GS) -> Module when
      GS :: #horus_gs{},
      Module :: module().

gen_module_name(#horus_gs{entrypoint = Entrypoint, functions = Functions}) ->
    #horus_fs{info = #{module := Module,
                       name := Name}} = maps:get(Entrypoint, Functions),
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

record_call(
  Module, Name, Arity, Type, FS,
  #horus_gs{functions = Functions, calls = Calls} = GS) ->
    CallRef = {Module, Name, Arity},
    GS2 = case Calls of
              #{CallRef := _} ->
                  GS;
              _ ->
                  {ShouldProcess, GS1} = should_process_function(
                                            Module, Name, Arity,
                                            FS, GS),
                  Calls1 = Calls#{CallRef => true},
                  case ShouldProcess of
                      true ->
                          Functions1 = Functions#{
                                         CallRef => {undefined, Type}
                                        },
                          GS1#horus_gs{
                            calls = Calls1,
                            functions = Functions1
                           };
                      false ->
                          GS1#horus_gs{
                            calls = Calls1
                           }
                  end
          end,
    GS2.

-spec should_process_function(Module, Name, Arity, FS, GS) ->
    {ShouldProcess, GS} when
      Module :: module(),
      Name :: atom(),
      Arity :: arity(),
      FS :: #horus_fs{},
      GS :: #horus_gs{},
      ShouldProcess :: boolean().

should_process_function(
  erl_eval, Name, Arity,
  #horus_fs{info = #{module := erl_eval,
                     name := Name,
                     arity := Arity,
                     type := local}},
  #horus_gs{} = GS) ->
    %% We want to process lambas loaded by `erl_eval'
    %% even though we wouldn't do that with the
    %% regular `erl_eval' API.
    {true, GS};
should_process_function(
  Module, Name, Arity,
  #horus_fs{info = #{module := FromModule}},
  #horus_gs{options = #{should_process_function := Callback},
            errors = Errors} = GS)
  when is_function(Callback) ->
    try
        % io:format(standard_error, "should proceed ~p:~p/~p: ...~n", [Module, Name, Arity]),
        ShouldProcess = Callback(Module, Name, Arity, FromModule),
        io:format(standard_error, "should proceed ~p:~p/~p: ~p~n", [Module, Name, Arity, ShouldProcess]),
        {ShouldProcess, GS}
    catch
        throw:Error ->
            Errors1 = Errors ++ [Error],
            GS1 = GS#horus_gs{errors = Errors1},
            {false, GS1}
    end;
should_process_function(Module, _Name, _Arity, _FromModule, GS) ->
    ShouldProcess = horus_utils:should_process_module(Module),
    {ShouldProcess, GS}.

-spec ensure_cerl_node_is_permitted(Node, GS) ->
    GS when
      Node :: cerl:cerl(),
      GS :: #horus_gs{}.

ensure_cerl_node_is_permitted(
  CNode,
  #horus_gs{options = #{ensure_cerl_node_is_permitted := Callback},
            errors = Errors} = GS)
  when is_function(Callback) ->
    try
        Callback(CNode),
        GS
    catch
        throw:Error ->
            % io:format(standard_error, "ensure_cerl_node_is_permitted = ~p~n", [Error]),
            Errors1 = Errors ++ [Error],
            GS#horus_gs{errors = Errors1};
        C:R:S ->
            % io:format("GS = ~p~n", [GS]),
            erlang:raise(C, R, S)
    end;
ensure_cerl_node_is_permitted(_CNode, GS) ->
    GS.

-spec is_standalone_fun_still_needed(GS) -> IsNeeded when
      GS :: #horus_gs{},
      IsNeeded :: boolean().

is_standalone_fun_still_needed(
  #horus_gs{options = #{is_standalone_fun_still_needed := Callback},
            calls = Calls,
            errors = Errors})
  when is_function(Callback) ->
    Callback(#{calls => Calls,
               errors => Errors});
is_standalone_fun_still_needed(_GS) ->
    true.

gen_fun_name_mapping(Functions) ->
    FunNameMapping = maps:fold(fun gen_fun_name_mapping1/3, #{}, Functions),
    FunNameMapping.

gen_fun_name_mapping1(
  MFA,
  #horus_fs{internal_name = Name, internal_arity = Arity},
  Acc)
  when is_tuple(MFA) ->
    Acc#{{Name, Arity} => MFA};
gen_fun_name_mapping1(
  Fun,
  #horus_fs{internal_name = Name, internal_arity = Arity, info = Info},
  Acc)
  when is_function(Fun) ->
    #{module := M,
      name := F,
      arity := A} = Info,
    Acc#{{Name, Arity} => {M, F, A}};
gen_fun_name_mapping1(
  _MFA,
  comprehension,
  Acc) ->
    Acc.

%% TODO: Return all errors?
process_errors(#horus_gs{errors = []}) ->
    ok;
process_errors(#horus_gs{errors = [Error | _]}) ->
    throw(?horus_error(extraction_denied, #{error => Error})).

%% -------------------------------------------------------------------
%% Environment handling.
%% -------------------------------------------------------------------

-spec to_standalone_env(FS, GS) -> {StandaloneEnv, NewGS} when
      FS :: #horus_fs{},
      GS :: #horus_gs{},
      StandaloneEnv :: list(),
      NewGS :: #horus_gs{}.
%% @doc Converts the fun environment to a standalone term.
%%
%% For "regular" lambdas, variables declared outside of the function body are
%% put in this `env'. We need to process them in case they reference other
%% lambdas for instance. We keep the end result to store it alongside the
%% generated module, but not inside the module to avoid an increase in the
%% number of identical modules with different environment.
%%
%% However for `erl_eval' functions created from lambdas, the env contains the
%% parsed source code of the function. We don't need to interpret it.
%%
%% TODO: `to_standalone_env()' uses `to_standalone_fun1()' to extract and
%% compile lambdas passed as arguments. It means they are fully compiled even
%% if `is_standalone_fun_still_needed()' returns false later. This is a waste
%% of resources and this function can probably be split into two parts to
%% allow the environment to be extracted before and compiled after, once we
%% are sure we need to create the final standalone fun.

to_standalone_env(
  #horus_fs{info = #{module := Module,
                     type := Type,
                     env := Env}},
  #horus_gs{options = Options} = GS)
  when Env =/= [] andalso (Module =/= erl_eval orelse Type =/= local) ->
    Options1 = maps:remove(is_standalone_fun_still_needed, Options),
    GS1 = GS#horus_gs{options = Options1},
    {Env1, GS2} = to_standalone_arg(Env, GS1),
    GS3 = GS2#horus_gs{options = Options},
    {Env1, GS3};
to_standalone_env(_FS, GS) ->
    {[], GS}.

to_standalone_arg(List, GS) when is_list(List) ->
    lists:foldr(
      fun(Item, {L, GS1}) when is_list(L) ->
              {Item1, GS2} = to_standalone_arg(Item, GS1),
              {[Item1 | L], GS2}
      end, {[], GS}, List);
to_standalone_arg(Tuple, GS) when is_tuple(Tuple) ->
    List0 = tuple_to_list(Tuple),
    {List1, GS1} = to_standalone_arg(List0, GS),
    Tuple1 = list_to_tuple(List1),
    {Tuple1, GS1};
to_standalone_arg(Map, GS) when is_map(Map) ->
    maps:fold(
      fun(Key, Value, {M, GS1}) ->
              {Key1, GS2} = to_standalone_arg(Key, GS1),
              {Value1, GS3} = to_standalone_arg(Value, GS2),
              M1 = M#{Key1 => Value1},
              {M1, GS3}
      end, {#{}, GS}, Map);
to_standalone_arg(Fun, GS) when is_function(Fun) ->
    to_embedded_standalone_fun(Fun, GS);
to_standalone_arg(Term, GS) ->
    {Term, GS}.

-spec to_embedded_standalone_fun(Fun, GS) -> {StandaloneFun, NewGS} when
      Fun :: fun(),
      GS :: #horus_gs{},
      StandaloneFun :: horus_fun(),
      NewGS :: #horus_gs{}.
%% @private
%% @hidden

to_embedded_standalone_fun(
  Fun,
  #horus_gs{options = Options,
            errors = Errors} = GS)
  when is_function(Fun) ->
    {StandaloneFun, InnerGS} = to_standalone_fun1(Fun, Options),
    #horus_gs{calls = InnerCalls,
              errors = InnerErrors} = InnerGS,
    Errors1 = Errors ++ InnerErrors,
    GS1 = merge_calls_maps(InnerCalls, GS),
    GS2 = GS1#horus_gs{errors = Errors1},
    {StandaloneFun, GS2}.

-spec merge_calls_maps(Calls, GS) -> NewGS when
      Calls :: calls_map(),
      GS :: #horus_gs{},
      NewGS :: #horus_gs{}.
%% @private

merge_calls_maps(InnerCalls, #horus_gs{calls = Calls} = GS) ->
    Calls1 = maps:merge(Calls, InnerCalls),
    GS1 = GS#horus_gs{calls = Calls1},
    GS1.
