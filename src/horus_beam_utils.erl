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
         get_abstract_code/1]).

-type beam() :: binary().

-export_type([beam/0]).

-spec get_beam(Module) -> Beam when
      Module :: module(),
      Beam :: horus_beam_utils:beam().

get_beam(Module) ->
    case code:get_object_code(Module) of
        {_Module, Beam, _Filename} ->
            Beam;
        error ->
            ?horus_misuse(
               module_not_found,
               #{module => Module})
    end.

-spec get_abstract_code(Beam) -> AbstractCode when
      Beam :: horus_beam_utils:beam(),
      AbstractCode :: beam_lib:abs_code().

get_abstract_code(Beam) when is_binary(Beam) ->
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
    end;
get_abstract_code(Module) when is_atom(Module) ->
    Beam = get_beam(Module),
    get_abstract_code(Beam).
