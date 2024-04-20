# Devcontainer for GHC WASM backend

A simple devcontainer to use GHC 9.10 (alpha) WASM backend, mainly designed to use with Apple Silicon macOS as a Host OS.

## Usage

1. Clone repository
2. Open directory with VSCode
3. Choose `Reopen in Dev Container`
4. Take a coffee break (takes ~10mins with MacBook Pro with Apple M3 Pro)
5. :yummy:

    ```bash
    ghc --make hello.hs && wasmtime hello.wasm
    ```
