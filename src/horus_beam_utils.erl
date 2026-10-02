%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus_beam_utils).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("src/horus_error.hrl").

-export([get_beam/1,
         get_abstract_code/1,
         get_abstract_code_from_beam/1]).

-type beam() :: binary().

-export_type([beam/0]).

-define(
   ABSTRACT_CODE_CACHE_KEY(Module, Checksum),
   {horus, abstract_code_cache, Module, Checksum}).

-spec get_abstract_code(Module) -> AbstractCode when
      Module :: module(),
      AbstractCode :: beam_lib:abs_code().

get_abstract_code(Module) when is_atom(Module) ->
    Checksum = Module:module_info(md5),
    CacheKey = ?ABSTRACT_CODE_CACHE_KEY(Module, Checksum),
    case persistent_term:get(CacheKey, undefined) of
        AbstractCode when is_list(AbstractCode) ->
            % io:format(standard_error, "------ Get AC ~p from cache~n", [Module]),
            AbstractCode;
        undefined ->
            % io:format(standard_error, "------ Get AC ~p from beam~n", [Module]),
            AbstractCode = do_get_abstract_code(Module, CacheKey),
            AbstractCode
    end.

do_get_abstract_code(Module, CacheKey) ->
    LockKey = {horus, abstract_code_lock, Module},
    Lock = {LockKey, self()},
    global:set_lock(Lock, [node()]),
    try
        case persistent_term:get(CacheKey, undefined) of
            AbstractCode when is_list(AbstractCode) ->
                AbstractCode;
            undefined ->
                AbstractCode = do_get_abstract_code_locked(Module),
                AbstractCode
        end
    after
        global:del_lock(Lock, [node()])
    end.

do_get_abstract_code_locked(Module) ->
    Beam = get_beam(Module),
    {ok, {Module, Checksum}} = beam_lib:md5(Beam),
    CacheKey = ?ABSTRACT_CODE_CACHE_KEY(Module, Checksum),
    AbstractCode = get_abstract_code_from_beam(Beam),
    persistent_term:put(CacheKey, AbstractCode),
    AbstractCode.

get_abstract_code_from_beam(Beam) when is_binary(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, AbstractCode}}]}} ->
            % logger:alert("Module ~s abstract code: ~p", [_Module, AbstractCode]),
            AbstractCode;
        _ ->
            BeamInfo = beam_lib:info(Beam),
            Props = case proplists:get_value(module, BeamInfo) of
                        undefined -> #{};
                        Module    -> #{module => Module}
                    end,
            ?horus_misuse(
               abstract_code_unavailable,
               Props)
    end.

-spec get_beam(Module) -> Beam when
      Module :: module(),
      Beam :: horus_beam_utils:beam().

get_beam(Module) ->
    % io:format(standard_error, "------ Get beam ~p~n", [Module]),
    case code:get_object_code(Module) of
        {_Module, Beam, _Filename} ->
            Beam;
        error ->
            ?horus_misuse(
               module_not_found,
               #{module => Module})
    end.
