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

% -spec get_fun_abstract_code(Fun) -> AbstractCode when
%       Fun :: fun(),
%       AbstractCode :: beam_lib:abs_code().
%
% get_fun_abstract_code(Fun) ->
%     FunInfo = maps:from_list(erlang:fun_info(Fun)),
%     #{module := Module,
%       name := _FunName,
%       arity := _Arity} = FunInfo,
%     StartLine = get_fun_start_line(Fun),
%     Beam = get_beam(Module),
%     AbstractCode = get_abstract_code(Beam),
%     logger:alert("AbstractCode = ~p", [AbstractCode]),
%     locate_function_abstract_code(AbstractCode, {'fun', StartLine}).

%% Make this scan function generic:
%% * stackable scopes + "instruction" to unstack a scope when exiting one (to
%%   maintain a tail-recursive scanning function)
%% * take a callback that can indicate if the scan should continue (e.g. stop
%%   the current expression/scope or stop entirely)
%% How to patch a function, like its calls?

%% TODO: Use fold/4.

% locate_function_abstract_code(
%   [{function, _Location, _FunName, _Arity, Clauses} | Rest],
%   Target) ->
%     locate_function_abstract_code(Clauses ++ Rest, Target);
% locate_function_abstract_code(
%   [{'fun', {StartLine, _StartCol}, _Clauses} = AbstractCode | _Rest],
%   {'fun', StartLine}) ->
%     AbstractCode;
% locate_function_abstract_code(
%   [{'fun', _Location, Clauses} | Rest],
%   Target) ->
%     locate_function_abstract_code(Clauses ++ Rest, Target);
% locate_function_abstract_code(
%   [{clause, _Location, _Arg, _Guards, Body} | Rest],
%   Target) ->
%     locate_function_abstract_code(Body ++ Rest, Target);
% locate_function_abstract_code(
%   [{match, _Location, Left, Right} | Rest],
%   Target) ->
%     locate_function_abstract_code([Left, Right | Rest], Target);
% locate_function_abstract_code(
%   [{var, _Location, _Name} | Rest],
%   Target) ->
%     locate_function_abstract_code(Rest, Target);
% locate_function_abstract_code(
%   [{atom, _Location, _Name} | Rest],
%   Target) ->
%     locate_function_abstract_code(Rest, Target);
% locate_function_abstract_code(
%   [{tuple, _Location, Elements} | Rest],
%   Target) ->
%     locate_function_abstract_code(Elements ++ Rest, Target);
% locate_function_abstract_code(
%   [{call, _Location, _Call, _Args} | Rest],
%   Target) ->
%     locate_function_abstract_code(Rest, Target);
% locate_function_abstract_code(
%   [{attribute, _, _, _} | Rest],
%   Target) ->
%     locate_function_abstract_code(Rest, Target);
% locate_function_abstract_code(
%   [], _Target) ->
%     false.
