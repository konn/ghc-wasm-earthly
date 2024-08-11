# GHC WASM backend building for macOS

A simple earthly-based build system for GHC 9.10 (alpha) WASM backend, mainly designed to use with (Apple Silicon) macOS as a Host OS.
This repository comes with a container image to use GHC WASM backend in handy:

- [`ghcr.io/konn/ghc-wasm-earthly`](https://github.com/users/konn/packages/container/package/ghc-wasm-earthly)

## `ghc-wasm-compat`: GHC WASM Compatibility Layer

The package `ghc-wasm-compat` provides a glue-code to make wasm-targeted Haskell code to type-check with vanilla (native) GHC.
Note that the purpose of this package is _NOT_ to run WASM-based code also on native - it just provides a dummy APIs and compiler plugin to replace FFI code with dummy code with the same type but errors at the runtime.
The main intended usage is to use HLS on WASM-targeted code.

`ghc-wasm-compat` provides two machineries.

### The `GHC.Wasm.Prim` module

In the WASM backend, this just re-exports the same module from `ghc-experimental`.
In other platforms, it provides a dummy API that errors at runtime.

### `GHC.Wasm.FFI.Plugin`

This module provides a source plugin to replace JavaScript FFI with dummy code in non-WASM backend.
This might sounds having similar purpose of jsaddle, but it have a different goal: jsaddle aims at makes code working even with native backend by using browser as a JS backend; `GHC.Wasm.FFI.Plugin`, on the other hand, only assumes that the code type-checks and compiles. The intended use case of the latter is to use HLS on WASM-targeted code.

It does nothing when used with the WASM backend.
In other backends, on the other hand, it removes all JS FFI exports and replaces JS FFI imports with the function definition with a dummy function definition with the same name and type but just raises an error at runtime.
  
Example usage:

```hs
{-# OPTIONS_GHC -fplugin GHC.Wasm.FFI.Plugin #-}
module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSString -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString

greet :: JSString -> IO ()
greet name = consoleLog $ "Hello, " <> fromJSString name

foreign export javascript "greet" greet :: JSString -> IO ()
```

When compiled with the WASM backend, this works just as expected.
The code also compiles with other backends without any change, but under the hood, the source plugin silently modifies the module to something like this:

```haskell
module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.Prim

js_console_log :: JSString -> IO ()
js_console_log = error "foreign import javascript unsafe \"console.log($1)\" js_console_log :: JSString -> IO ()"

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString

greet :: JSString -> IO ()
greet name = consoleLog $ "Hello, " <> fromJSString name
```

Beware that the `foreign export` of `greet` has gone and the definition of `js_console_log` is replaced with the runtime error.

### Note on Plugins with WASM

Currently (April 2024), WASM backend doesn't provide an interactive REPL (GHCi), which prevents Template Haskell and compiler plugin from working with WASM backend.
The lack of Template Haskell leads to the development of this module as a source plugin - because no macro is needed and does not sacrifices well-formedness of the programs.

Still, WASM backend in GHC 9.10 doesn't support GHC plugin either. So, you can just enable source plugin only when WASM backend is not used. We recommend to conditionally specify `-fplugin` option in cabal file, like this:

```cabal
library
  if !os(wasi)
    ghc-options: -fplugin GHC.Wasm.FFI.Plugin
    build-depends: ghc-wasm-compat
```

With this, `GHC.Wasm.FFI.Plugin` is enabled only with non-WASM backends.
As the implementation of the plugin for WASM backend still exists and it is no-op, it can be enabled unconditionally once WASM backends supports GHC plugins in near future.

## Demos

### Demo 1: simple WASI app

1. Install [Earthly](https://earthly.dev).
2. Clone repository
3. Run `earthly +hello`
4. :yum:

    ```bash
    $ wasmtime ./_build/hello.wasm
    Hello, WASM World from GHC 9.10!
    ```

### Demo 2: Calling JS FFI

[`wasm-jsffi-ghc-demo/` ](./wasm-jsffi-ghc-demo) contains a simple example to call the `console.log` JS function from Haskell.
To run:

```bash
$ earthly +hello-js
...

$ cd ./_build/console-log/

$ deno run --allow-read run.ts console-log.wasm
Hello, world! (cabalised, JS FFI!)
```

## On IDEs

As of 2024-04-20, HLS doesn't compile with GHC 9.10 (even if almost all plugins are disabled and using `head.hackage`).
It might not be too hard to fix it, but I would rather use GHC 9.8 on the host.
The situation should be resolved once HLS supports GHC 9.10.

## Notes on Routers for Cloudflare Worker

Although this repository contains some implementation of a Servant-Workers bridge, it is not encouraged to use it.
It currently converts ReadableStream from/to Haskell-side streaming mechanisms to use with WAI-types, which makes interop with R2 and/or Caching API of Workers fragile.
We decided to develop Steward, which is a subset of Servant's Generic interface and designed (incompletely) to work both with Workers and client.

## Prior Works

### GHC WASM Backend + JSFFI

A much more involved example targeting browsers is already provided by Tweag guys:

https://github.com/tweag/ghc-wasm-miso-examples?tab=readme-ov-file

Indeed, the vast majority of the build script of my repository is stolen from there.
Tweag's code requires Nix on Linux with x86_64 arch, so it cannot be run on Apple Silicon macOS directly.

Tweag's repository also lacks tricks needed to run with CLI JS/TS runtime such as deno, bun, or Node.js.
My project targets Deno.

### Earthly

@Lugendre [uses](https://github.com/Lugendre/earthly-haskell) Earthly to build static binaries with Earthly:

https://github.com/Lugendre/earthly-haskell

### Cloudflare Worker with Haskell

[Stack Builders](https://blog.cloudflare.com/cloudflare-worker-with-webassembly-and-haskell) already achieved this with Asterius.

