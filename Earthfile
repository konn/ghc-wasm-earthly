VERSION 0.8
FROM DOCKERFILE --platform=linux/amd64 -f ./Dockerfile -
WORKDIR /workdir

base-image:
  SAVE IMAGE ghc-wasm-earthly:9.10.0.20240413

hello:
  COPY --keep-ts ./hello/hello.hs .
  RUN wasm32-wasi-ghc --make hello.hs
  SAVE ARTIFACT hello.wasm AS LOCAL _build/hello.wasm
