VERSION 0.8
FROM DOCKERFILE --platform=linux/amd64 -f ./Dockerfile -
WORKDIR /workdir

docs:
  SAVE ARTIFACT /root/.ghcup/ghc/wasm32-wasi-9.10.0.20240412/share/doc/wasm32-wasi-ghc-9.10.0.20240412/ AS LOCAL _build/docs

base-image:
  SAVE IMAGE ghc-wasm-earthly:9.10.0.20240413

hello:
  COPY --keep-ts ./hello/hello.hs .
  RUN cat /root/.ghc-wasm/env
  RUN wasm32-wasi-ghc --make hello.hs
  SAVE ARTIFACT hello.wasm AS LOCAL _build/hello.wasm
