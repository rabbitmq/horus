%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2022-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-define(
   horus_error(Name, Props),
   {horus, Name, Props}).

-define(
   horus_exception(Name, Props),
   {horus_ex, Name, Props}).

-define(
   horus_misuse(Exception),
   erlang:error(Exception)).

-define(
   horus_misuse(Name, Props),
   ?horus_misuse(?horus_exception(Name, Props))).

-define(
   horus_raise_misuse(Name, Props, Stacktrace),
   erlang:raise(error, ?horus_exception(Name, Props), Stacktrace)).
