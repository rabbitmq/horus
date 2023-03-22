%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-ifndef(HORUS_FUN_HRL).
-define(HORUS_FUN_HRL, true).

%% Structure representing an anonymous function "extracted" as a compiled
%% module for storage.
%%
%% IMPORTANT: When adding or removing fields to this record, be sure to update
%% `include/horus.hrl'!
-record(horus_fun, {module :: module(),
                    beam :: binary(),
                    arity :: arity(),
                    literal_funs :: [horus:horus_fun()],
                    env :: list(),
                    debug_info :: horus:debug_info() | undefined}).

-endif. % defined(HORUS_FUN_HRL).
