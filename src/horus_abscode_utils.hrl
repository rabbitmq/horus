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
-record(block, {location, expressions}).
-record(call, {location, call, args}).
-record('case', {location, input, clauses}).
-record(char, {location, value}).
-record(clause, {location, args, guards, body}).
-record(clauses, {clauses}).
-record(cons, {location, head, tail}).
-record(eof, {location}).
-record(float, {location, value}).
-record('fun', {location, code}).
-record(function, {location, name, arity, clauses}).
-record('if', {location, clauses}).
-record(integer, {location, value}).
-record(map, {location, elements}).
-record(map_field_assoc, {location, key, value}).
-record(map_field_exact, {location, key, value}).
-record(match, {location, left, right}).
-record(nil, {location}).
%% The `{op, ...}' has a variable number of operands and can't be a record.
% -record(op, {location, operator, operand}).
% -record(op, {location, operator, operand1, operand2}).
-record('receive', {location, clauses}).
-record(record, {location, name, fields}).
-record(record_field, {location, name, value}).
-record(remote, {location, module, function}).
-record(string, {location, value}).
-record('try', {location, block, unnamed1, 'catch', unnamed2}).
-record(tuple, {location, elements}).
-record(var, {location, name}).
