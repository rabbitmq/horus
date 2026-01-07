%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2024-2026 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-if(?OTP_RELEASE >= 27).
-define(IF_NATIVE_COVERAGE_IS_SUPPORTED(IfSupportedBlock, ElseBlock),
        (case code:coverage_support() of
             true ->
                 IfSupportedBlock;
             false ->
                 ElseBlock
         end)).
-else.
-define(IF_NATIVE_COVERAGE_IS_SUPPORTED(_IfSupportedBlock, ElseBlock),
        (ElseBlock)).
-endif.
