%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-ifndef(HORUS_HRL).
-define(HORUS_HRL, true).

-define(IS_HORUS_FUN(Fun),
        (is_function(Fun) orelse ?IS_HORUS_STANDALONE_FUN(Fun))).

-define(IS_HORUS_STANDALONE_FUN(Fun),
        (is_tuple(Fun) andalso
         size(Fun) =:= 8 andalso
         element(1, Fun) =:= horus_fun)).

-define(IS_HORUS_STANDALONE_FUN(Fun, Arity),
        (?IS_HORUS_STANDALONE_FUN(Fun) andalso
         ?HORUS_STANDALONE_FUN_ARITY(Fun) =:= Arity)).

-define(HORUS_STANDALONE_FUN_ARITY(Fun),
        element(4, Fun)).

-define(horus_error(Name, Props), {horus, Name, Props}).
-define(horus_exception(Name, Props), {horus_ex, Name, Props}).

-endif. % defined(HORUS_HRL).
