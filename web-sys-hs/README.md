# web-sys-hs

## How to generate modules?

Use `../webidl-codegen-wasm` and cabal-gild to generate modules from webidls.

```bash
cd web-sys-hs
cabal run webidl-codegen-wasm
cabal-gild --io web-sys-hs.cabal
```

## Copyright

2024 (c) Hiromi ISHII All Rights Reserved.
