VERSION 0.8
FROM DOCKERFILE --platform=linux/amd64 -f ./Dockerfile -
WORKDIR /workdir
ENV CABAL=wasm32-wasi-cabal --with-ghc=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs

base-image:
  SAVE IMAGE ghc-wasm-earthly:9.10.0.20240413

hello:
  COPY --keep-ts ./hello/hello.hs .
  RUN wasm32-wasi-ghc --make hello.hs
  SAVE ARTIFACT hello.wasm AS LOCAL _build/hello.wasm


hello-js:
  ARG TARGET=wasm-jsffi-ghc-demo:exe:console-log
  COPY --keep-ts . .
  RUN ${CABAL} build ${TARGET}
  SAVE ARTIFACT $(${CABAL} list-bin ${TARGET}) AS LOCAL _build/console-log.wasm
