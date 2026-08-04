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
    AbstractCode = case Module of
                       erl_eval ->
                           erl_eval_fun_to_abstract_code(Module, Name, Arity, Env);
                       _ ->
                           horus_beam_utils:get_abstract_code(Module)
                   end,
    % io:format(standard_error, "FunInfo = ~p~nAbstract Code = ~p~n", [FunInfo, AbstractCode]),
    if
        Type =:= local andalso Module =/= erl_eval ->
            do_get(Fun, {Name, Arity}, AbstractCode);
        Type =:= external orelse Module =:= erl_eval ->
            do_get({Module, Name, Arity}, {Name, Arity}, AbstractCode)
    end;
get({Module, Name, Arity} = MFA) ->
    ?LOG_DEBUG(
       "Horus: get Core Erlang for function ~s:~s/~b",
       [Module, Name, Arity]),
    AbstractCode = horus_beam_utils:get_abstract_code(Module),
    do_get(MFA, {Name, Arity}, AbstractCode).

-spec erl_eval_fun_to_abstract_code(Module, Name, Arity, Env) -> AbstractCode when
      Module :: module(),
      Name :: atom(),
      Arity :: arity(),
      Env :: any(),
      AbstractCode :: beam_lib:abs_code().
%% @private

erl_eval_fun_to_abstract_code(Module, Name, Arity, [{_, Bindings, _, _, _, Clauses}])
  when Bindings =:= [] orelse %% Erlang is using a list for bindings,
       Bindings =:= #{} ->    %% but Elixir is using a map.
    %% Erlang starting from 25.
    erl_eval_fun_to_abstract_code1(Module, Name, Arity, Clauses);
erl_eval_fun_to_abstract_code(Module, Name, Arity, [{Bindings, _, _, Clauses}])
  when Bindings =:= [] orelse %% Erlang is using a list for bindings,
       Bindings =:= #{} ->    %% but Elixir is using a map.
    %% Erlang up to 24.
    erl_eval_fun_to_abstract_code1(Module, Name, Arity, Clauses).

erl_eval_fun_to_abstract_code1(Module, Name, Arity, Clauses) ->
    %% We construct an abstract form based on the `env' of the lambda loaded
    %% by `erl_eval'.
    Anno = erl_anno:from_term(1),
    Forms = [{attribute, Anno, module, Module},
             {attribute, Anno, export, [{Name, Arity}]},
             {function, Anno, Name, Arity, Clauses}],
    Forms.

do_get(Reference, Target, AbstractCode) ->
    CompilerOptions = [binary,
                       to_core,
                       warnings_as_errors,
                       return_errors,
                       return_warnings,
                       deterministic],
    {ok, _, ModuleCoreErlang, _} = compile:forms(
                                     AbstractCode, CompilerOptions),
    % io:format(standard_error, "CORE ERLANG:~n~p~n", [ModuleCoreErlang]),
    do_get1(Reference, Target, ModuleCoreErlang).

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
