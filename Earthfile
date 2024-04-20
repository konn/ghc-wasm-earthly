VERSION 0.8
FROM DOCKERFILE --platform=linux/amd64 -f ./Dockerfile -
WORKDIR /workdir

hello:
  COPY --keep-ts . .
  RUN env
  RUN cat /root/.ghc-wasm/env
  RUN wasm32-wasi-ghc --make hello.hs
  SAVE ARTIFACT hello.wasm AS LOCAL _build/hello.wasm
