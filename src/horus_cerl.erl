%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_cerl).

-include_lib("kernel/include/logger.hrl").

-export([get/1,
         format/1]).

%% -------------------------------------------------------------------
%% get/1.
%% -------------------------------------------------------------------

-spec get(Fun | MFA) -> CoreErlang when
      Fun :: fun(),
      MFA :: {Module, Name, Arity},
      Module :: module(),
      Name :: atom(),
      Arity :: non_neg_integer(),
      CoreErlang :: cerl:cerl().
%% @doc Returns the Core Erlang code of the given function of MFA.

get(Fun) when is_function(Fun) ->
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module,
      name := Name,
      arity := Arity,
      type := Type,
      env := Env} = FunInfo,
    ?LOG_DEBUG(
       "Horus: get Core Erlang for function ~s:~s/~b",
       [Module, Name, Arity]),
    CErl = case Module of
                       erl_eval ->
                           erl_eval_fun_to_core_erlang(Module, Name, Arity, Env);
                       _ ->
                           get_core_erlang(Module)
                   end,
    % io:format(standard_error, "FunInfo = ~p~nAbstract Code = ~p~n", [FunInfo, AbstractCode]),
    if
        Type =:= local andalso Module =/= erl_eval ->
            do_get(Fun, {Name, Arity}, CErl);
        Type =:= external orelse Module =:= erl_eval ->
            do_get({Module, Name, Arity}, {Name, Arity}, CErl)
    end;
get({Module, Name, Arity} = MFA) ->
    ?LOG_DEBUG(
       "Horus: get Core Erlang for function ~s:~s/~b",
       [Module, Name, Arity]),
    CErl = get_core_erlang(Module),
    do_get(MFA, {Name, Arity}, CErl).

-spec erl_eval_fun_to_core_erlang(Module, Name, Arity, Env) -> CErl when
      Module :: module(),
      Name :: atom(),
      Arity :: arity(),
      Env :: any(),
      CErl :: cerl:cerl().
%% @private

erl_eval_fun_to_core_erlang(Module, Name, Arity, [{_, Bindings, _, _, _, Clauses}])
  when Bindings =:= [] orelse %% Erlang is using a list for bindings,
       Bindings =:= #{} ->    %% but Elixir is using a map.
    %% Erlang starting from 25.
    erl_eval_fun_to_core_erlang1(Module, Name, Arity, Clauses);
erl_eval_fun_to_core_erlang(Module, Name, Arity, [{Bindings, _, _, Clauses}])
  when Bindings =:= [] orelse %% Erlang is using a list for bindings,
       Bindings =:= #{} ->    %% but Elixir is using a map.
    %% Erlang up to 24.
    erl_eval_fun_to_core_erlang1(Module, Name, Arity, Clauses).

erl_eval_fun_to_core_erlang1(Module, Name, Arity, Clauses) ->
    %% We construct an abstract form based on the `env' of the lambda loaded
    %% by `erl_eval'.
    Anno = erl_anno:from_term(1),
    Forms = [{attribute, Anno, module, Module},
             {attribute, Anno, export, [{Name, Arity}]},
             {function, Anno, Name, Arity, Clauses}],
    % io:format(standard_error, "------ Compile AC ~p~n", [Module]),
    CErl = compile_abstract_code(Forms),
    CErl.

-define(
   CORE_ERLANG_CACHE_KEY(Module, Checksum),
   {horus, core_erlang_cache, Module, Checksum}).

get_core_erlang(Module) when is_atom(Module) ->
    Checksum = Module:module_info(md5),
    CacheKey = ?CORE_ERLANG_CACHE_KEY(Module, Checksum),
    case persistent_term:get(CacheKey, undefined) of
        CErl when is_tuple(CErl) ->
            % io:format(standard_error, "------ Get AC ~p from cache~n", [Module]),
            CErl;
        undefined ->
            % io:format(standard_error, "------ Get AC ~p from beam~n", [Module]),
            CErl = do_get_core_erlang(Module, CacheKey),
            CErl
    end.

do_get_core_erlang(Module, CacheKey) ->
    LockKey = {horus, core_erlang_lock, Module},
    Lock = {LockKey, self()},
    global:set_lock(Lock, [node()]),
    try
        case persistent_term:get(CacheKey, undefined) of
            CErl when is_tuple(CErl) ->
                CErl;
            undefined ->
                CErl = do_get_core_erlang_locked(Module),
                CErl
        end
    after
        global:del_lock(Lock, [node()])
    end.

do_get_core_erlang_locked(Module) ->
    Beam = horus_beam_utils:get_beam(Module),
    AbstractCode = horus_beam_utils:get_abstract_code_from_beam(Beam),
    {ok, {Module, Checksum}} = beam_lib:md5(Beam),
    CacheKey = ?CORE_ERLANG_CACHE_KEY(Module, Checksum),
    CErl = compile_abstract_code(AbstractCode),
    persistent_term:put(CacheKey, CErl),
    CErl.

compile_abstract_code(AbstractCode) ->
    CompilerOptions = [binary,
                       to_core,
                       warnings_as_errors,
                       return_errors,
                       return_warnings,
                       deterministic],
    {ok, _, ModuleCoreErlang, _} = compile:forms(
                                     AbstractCode, CompilerOptions),
    ModuleCoreErlang.

do_get(Reference, Target, CErl) ->
    % io:format(standard_error, "CORE ERLANG:~n~p~n", [CErl]),
    do_get1(Reference, Target, CErl).

do_get1(Reference, Target, ModuleCoreErlang) when is_function(Reference) ->
    PreCallback = fun(Node, _Fold, undefined = Priv) ->
                          case cerl:type(Node) of
                              'fun' ->
                                  Ann = cerl:get_ann(Node),
                                  case lists:keyfind(id, 1, Ann) of
                                      {id, {_, _, ThisName}} ->
                                          ThisArity = cerl:fun_arity(Node),
                                          case {ThisName, ThisArity} of
                                              Target -> {stop, Node};
                                              _      -> {in, Priv}
                                          end;
                                      false ->
                                          case lists:keyfind(function, 1, Ann) of
                                              {function, Target} ->
                                                  {stop, Node};
                                              _ ->
                                                  {in, Priv}
                                          end
                                  end;
                              _ ->
                                  {in, Priv}
                          end
                  end,
    case horus_cerl_fold:fold(ModuleCoreErlang, PreCallback, none, undefined) of
        {interrupted, FunCoreErlang} ->
            {ok, FunCoreErlang};
        _ ->
            throw({function_not_found, Reference, Target, ModuleCoreErlang})
    end;
do_get1(Reference, Target, ModuleCoreErlang) when is_tuple(Reference) ->
    PreCallback = fun(Node, _Fold, undefined = Priv) ->
                          case cerl:type(Node) of
                              module ->
                                  Definitions = cerl:module_defs(Node),
                                  case find_ref(Definitions, Target) of
                                      undefined ->
                                          {in, Priv};
                                      FunCoreErlang ->
                                          {stop, FunCoreErlang}
                                  end;
                              letrec ->
                                  Definitions = cerl:letrec_defs(Node),
                                  case find_ref(Definitions, Target) of
                                      undefined ->
                                          {in, Priv};
                                      FunCoreErlang ->
                                          {stop, FunCoreErlang}
                                  end;
                              _ ->
                                  {in, Priv}
                          end
                  end,
    case horus_cerl_fold:fold(ModuleCoreErlang, PreCallback, none, undefined) of
        {interrupted, FunCoreErlang} ->
            {ok, FunCoreErlang}
    end.

find_ref([{Var, FunCoreErlang} | Rest], Target) ->
    case cerl:var_name(Var) of
        Target -> FunCoreErlang;
        _      -> find_ref(Rest, Target)
    end;
find_ref([], _Target) ->
    undefined.

%% -------------------------------------------------------------------
%% Other APIs.
%% -------------------------------------------------------------------

format(CoreErlang) ->
    Txt = core_pp:format(CoreErlang),
    io:format(standard_error, "~s~n", [Txt]).
