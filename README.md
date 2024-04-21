# GHC WASM backend building for macOS

A simple earthly-based build system for GHC 9.10 (alpha) WASM backend, mainly designed to use with (Apple Silicon) macOS as a Host OS.
This repository comes with a container image to ease GHC

## Usage

1. Install [Earthly](https://earthly.dev).
2. Clone repository
3. Run `earthly +hello`
4. :yum:

    ```bash
    $ wasmtime ./_build/hello.wasm
    Hello, WASM World from GHC 9.10!
    ```

## DEMO: Calling JS FFI

[`wasm-jsffi-ghc-demo/` ](./wasm-jsffi-ghc-demo) contains a simple example to call JS' `console.log` function from Haskell.
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

## TODOs

- Glue code for non-wasm GHC (JSFFI and `GHC.Wasm.Prim` is only available in WASM backend, not native one)
  + We can't use Template Haskell with WASM backend for the time being - so some kinda external preprocessor would be needed to generate dummy codes for non-WASM compilers.

## Prior Works

### GHC WASM Backend + JSFFI

A much more involved example targeting browsers is already provided by Tweag guys:

https://github.com/tweag/ghc-wasm-miso-examples?tab=readme-ov-file

Indeed, the vast majority of the build script of my repository is stolen from there.
Tweag's code requires Nix on Linux with x86_64 arch, so it cannot be run on Apple Silicon macOS directly.

Tweag's repository also lacks tricks needed to run with CLI JS/TS runtime such as deno, bun, or Node.js.
My project targets deno.

### Earthly

@Lugendre [uses](https://github.com/Lugendre/earthly-haskell) Earthly to build static binary with Earthly (and utilises caching mechanism of Earthly):

https://github.com/Lugendre/earthly-haskell
