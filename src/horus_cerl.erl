%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_cerl).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([get/1,
         format/1]).

get(Fun) when is_function(Fun) ->
    FunInfo = horus_erlfun_utils:info(Fun),
    #{module := Module,
      name := Name,
      arity := Arity,
      type := Type} = FunInfo,
    AbstractCode = horus_beam_utils:get_abstract_code(Module),
    case Type of
        local    -> do_get(Fun, {Name, Arity}, AbstractCode);
        external -> do_get({Module, Name, Arity}, {Name, Arity}, AbstractCode)
    end;
get({Module, Name, Arity} = MFA) ->
    AbstractCode = horus_beam_utils:get_abstract_code(Module),
    do_get(MFA, {Name, Arity}, AbstractCode).

do_get(Reference, Target, AbstractCode) ->
    CompilerOptions = [binary,
                       to_core,
                       warnings_as_errors,
                       return_errors,
                       return_warnings,
                       deterministic],
    {ok, _, ModuleCoreErlang, _} = compile:forms(
                                     AbstractCode, CompilerOptions),
    do_get1(Reference, Target, ModuleCoreErlang).

do_get1(Reference, Target, ModuleCoreErlang) when is_function(Reference) ->
    % io:format(standard_error, "Get ~0p~n", [Reference]),
    % ?LOG_ALERT("Module Core Erlang: ~p", [ModuleCoreErlang]),
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
                                          {in, Priv}
                                  end;
                              _ ->
                                  {in, Priv}
                          end
                  end,
    case horus_cerl_fold:fold(ModuleCoreErlang, PreCallback, none, undefined) of
        {interrupted, FunCoreErlang} ->
            {ok, FunCoreErlang}
    end;
do_get1(Reference, Target, ModuleCoreErlang) when is_tuple(Reference) ->
    % ?LOG_ALERT("Module Core Erlang: ~p", [ModuleCoreErlang]),
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

format(CoreErlang) ->
    Txt = core_pp:format(CoreErlang),
    io:format(standard_error, "~s~n", [Txt]).
