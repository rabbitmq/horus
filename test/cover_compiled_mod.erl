%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(cover_compiled_mod).

-export([run/1]).

run(Arg) ->
    Hash = erlang:phash2(Arg),
    {ok, Hash}.
