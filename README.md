# Horus: anonymous function to standalone module

[![Hex.pm](https://img.shields.io/hexpm/v/horus)](https://hex.pm/packages/horus/)
[![Test](https://github.com/rabbitmq/horus/actions/workflows/test.yaml/badge.svg)](https://github.com/rabbitmq/horus/actions/workflows/test.yaml)
[![Codecov](https://codecov.io/gh/rabbitmq/horus/branch/main/graph/badge.svg?token=R0OGKZ2RK2)](https://codecov.io/gh/rabbitmq/horus)

Horus is a library that extracts an anonymous function's code and creates a
standalone version of it in a new module at runtime.

The goal is to have a storable and transferable function which does not depend
on the availability of the module that defined it.

<img align="right" height="150" src="/doc/horus-logo.svg">

## How does it work?

To achieve that goal, Horus extracts the assembly code of the anonymous
function and creates a standalone Erlang module based on it. This module can be
stored, transfered to another Erlang node and executed anywhere without the
presence of the initial anonymous function's module.

## Project maturity

Horus is still under active development and should be considered *Alpha* at
this stage.

## Documentation

* A short tutorial in the [Getting started](#getting-started) section below
* [Documentation and API reference](https://rabbitmq.github.io/horus/)

## Getting started

### Add as a dependency

Add Horus as a dependency of your project:

Using Rebar:

```erlang
%% In rebar.config
{deps, [{horus, "0.2.2"}]}.
```

Using Erlang.mk:

```make
# In your Makefile
DEPS += horus
dep_horus = hex 0.2.2
```

Using Mix:

```elixir
# In mix.exs
defp deps do
  [
    :horus, "0.2.2"}
  ]
end
```

### Extract an anonymous function

To extract an anonymous function, use `horus:to_standalone_fun/1`:

```erlang
Fun = fun() ->
          do_something_fancy()
      end,

StandaloneFun = horus:to_standalone_fun(Fun).
```

It works with references to regular functions are well:

```erlang
Log = fun logger:info/2,

StandaloneLog = horus:to_standalone_fun(Log).
```

### Execute a standalone function

Once extracted, the function can be stored as an Erlang binary, or transfered
to a remote Erlang node. You then use `horus:exec/2` to execute it:

```erlang
receive
    {standalone_fun, StandaloneLog} ->
        horus:exec(
          StandaloneLog,
          ["~p received and executed function", [self()]])
end.
```

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
