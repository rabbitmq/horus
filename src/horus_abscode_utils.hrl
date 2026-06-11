%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

%% Erlang abstract code expressions.

-record(atom, {location, name}).
-record(attribute, {location, name, value}).
-record(bin, {location, elements}).
-record(bin_element, {location, value, size, specifiers}).
-record(call, {location, call, args}).
-record(clause, {location, args, guards, body}).
-record(cons, {location, head, tail}).
-record(eof, {location}).
-record('fun', {location, props}).
-record(function, {location, name, arity, clauses}).
-record(integer, {location, value}).
-record(match, {location, left, right}).
-record(nil, {location}).
-record(op, {location, operator, value}).
-record(string, {location, value}).
-record(tuple, {location, elements}).
-record(var, {location, name}).
