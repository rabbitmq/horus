%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(horus4).

-include_lib("stdlib/include/assert.hrl").

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

-record(horus_gs, {entrypoint :: horus4:fun_ref(),
                   options = #{} :: horus4:options(),

                   functions :: #{horus4:fun_ref() => #horus_fs{} | undefined}
                  }).

-type options() :: #{% should_process_function =>
                     % should_process_function_fun(),
                     %
                     % is_standalone_fun_still_needed =>
                     % is_standalone_fun_still_needed_fun(),

                     compile_options => list()}.

-type fun_ref() :: fun() | {module(), atom(), arity()}.

-export_type([options/0,
              fun_ref/0]).

-define(SF_ENTRYPOINT, run).

to_standalone_fun(Entrypoint, Options) ->
    GS = #horus_gs{entrypoint = Entrypoint,
                   options = Options,

                   functions = #{Entrypoint => undefined}
                  },
    extract_next_unprocessed_function(GS).

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
    ?assertEqual(undefined, maps:get(MFA, Functions)),
    Info = #{module => Module,
             name => Name,
             arity => Arity,
             type => external,
             env => []},
    FS = #horus_fs{reference = MFA,
                   info = Info},
    do_extract(FS, GS).

do_extract(FS, GS) ->
    case can_extract(FS, GS) of
        true ->
            do_extract1(FS, GS);
        false ->
            % XXX
            throw(pouet)
    end.

can_extract(
  #horus_fs{info = #{type := local}},
  _GS) ->
    true;
can_extract(
  #horus_fs{info = #{type := external,
                     module := Module,
                     name := Name,
                     arity := Arity}},
  _GS) ->
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
  #horus_gs{entrypoint = Entrypoint,
            functions = Functions} = GS) ->
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
                              internal_arity = InternalArity},
            Functions1 = Functions#{Reference => FS1},
            GS1 = GS#horus_gs{functions = Functions1},
            do_extract2(Reference, CErl, GS1);
        {error, _} = Error ->
            % XXX
            throw(Error)
    end.

do_extract2(Reference, CErl, GS) ->
    PreCallback = fun extract_pre_callback/3,
    PostCallback = fun extract_post_callback/3,
    InitialVars = #{},
    FunDepths = [],
    Priv = {InitialVars, FunDepths, GS},
    case horus_cerl_fold:fold(CErl, PreCallback, PostCallback, Priv) of
        {ok, CErl1, Priv1} ->
            {_Vars,
             _FunDepths,
             #horus_gs{functions = #{Reference := #horus_fs{} = FS} = Functions} = GS1
            } = Priv1,
            FS1 = FS#horus_fs{core_erlang = CErl1},
            Functions1 = Functions#{Reference => FS1},
            GS2 = GS1#horus_gs{functions = Functions1},
            extract_next_unprocessed_function(GS2);
        {error, _} = Error ->
            % XXX
            throw(Error)
    end.

extract_pre_callback(_CNode, _Fold, Priv) ->
    {in, Priv}.

extract_post_callback(CNode, _Fold, Priv) ->
    {ok, CNode, Priv}.

create_standalone_fun(GS) ->
    GS.

-spec gen_function_name(Module, Name) -> Name when
      Module :: module(),
      Name :: atom().

gen_function_name(Module, Name) ->
    InternalName = lists:flatten(
                     io_lib:format(
                       "~s__~s", [Module, Name])),
    list_to_atom(InternalName).
