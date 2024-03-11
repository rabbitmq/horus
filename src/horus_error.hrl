%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2022-2024 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-ifndef(HORUS_ERROR_HRL).
-define(HORUS_ERROR_HRL, true).

-include("include/horus.hrl").

-define(
   horus_misuse(Exception),
   erlang:error(Exception)).

-define(
   horus_misuse(Name, Props),
   ?horus_misuse(?horus_exception(Name, Props))).

-define(
   horus_raise_misuse(Name, Props, Stacktrace),
   erlang:raise(error, ?horus_exception(Name, Props), Stacktrace)).

-endif. % defined(HORUS_ERROR_HRL).
