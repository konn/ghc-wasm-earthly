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
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  ENV HS_WASM_PATH=$(${CABAL} list-bin ${TARGET})
  ENV WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  ENV DEST=dist/console-log.wasm
  RUN mkdir -p dist
  RUN ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ghc_wasm_jsffi.js
  RUN wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/console-log.wasm "${HS_WASM_PATH}"
  RUN wasm-opt -Oz dist/console-log.wasm -o dist/console-log.wasm
  RUN wasm-tools strip -o dist/console-log.wasm dist/console-log.wasm
  RUN cp wasm-jsffi-ghc-demo/data/run.ts dist/run.ts
  RUN cp *.js dist/

  # Use deno to run the console-log demo
  RUN cd dist && deno run --allow-read ./run.ts ./console-log.wasm

  SAVE ARTIFACT ./dist AS LOCAL _build/console-log
