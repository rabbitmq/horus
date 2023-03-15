# Horus: anonymous function to standalone module

[![Hex.pm](https://img.shields.io/hexpm/v/horus)](https://hex.pm/packages/horus/)
[![Test](https://github.com/rabbitmq/horus/actions/workflows/test.yaml/badge.svg)](https://github.com/rabbitmq/horus/actions/workflows/test.yaml)
[![Codecov](https://codecov.io/gh/rabbitmq/horus/branch/main/graph/badge.svg?token=R0OGKZ2RK2)](https://codecov.io/gh/rabbitmq/horus)

Horus is a library that extracts an anonymous function's code and creates a
standalone version of it in a new module at runtime.

## Project maturity

Horus is still under active development and should be considered *Alpha* at
this stage.

## How to build

### Build

```
rebar3 compile
```

### Build documentation

```
rebar3 edoc
```

### Test

```
rebar3 xref
rebar3 eunit
rebar3 ct --sname ct
rebar3 as test dialyzer
```

## Copyright and License

© 2021-2023 VMware, Inc. or its affiliates.

This work is dual-licensed under the Apache License 2.0 and the Mozilla Public
License 2.0. You can choose between one of them if you use this work.

SPDX-License-Identifier: Apache-2.0 OR MPL-2.0
