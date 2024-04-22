VERSION 0.8
ARG --global GHC_VER=9.10.0.20240412
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project --with-ghc=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs

base-image:
  FROM DOCKERFILE --platform=linux/amd64 --build-arg GHC=${GHC_VER} -f ./Dockerfile -
  SAVE IMAGE --push ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}

hello:
  COPY --keep-ts ./hello/hello.hs .
  RUN wasm32-wasi-ghc --make hello.hs
  SAVE ARTIFACT hello.wasm AS LOCAL _build/hello.wasm

BUILD:
  FUNCTION
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  ENV MOUNT_GLOBAL_STORE="type=cache,mode=0777,id=${target}#ghc-${GHC_VER}#global-store,sharing=shared,target=/root/.ghc-wasm/.cabal/store"
  ENV MOUNT_DIST_NEWSTYLE="type=cache,mode=0777,id=${target}#ghc${GHC_VER}#dist-newstyle,sharing=shared,target=dist-newstyle"
  COPY --keep-ts . .
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build --only-dependencies ${target}
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build  ${target}
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist/${wasm}
  RUN mkdir -p dist
  RUN --mount ${MOUNT_DIST_NEWSTYLE} ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ghc_wasm_jsffi.js
  RUN --mount ${MOUNT_DIST_NEWSTYLE} wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} "${HS_WASM_PATH}"
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  RUN cp wasm-jsffi-ghc-demo/data/run.ts dist/run.ts
  RUN cp *.js dist/

  # Use deno to run the console-log demo
  RUN cd dist && deno run --allow-read ./run.ts ./${wasm}

  SAVE ARTIFACT ./dist/ AS LOCAL _build/${outdir}/

hello-js:
  DO +BUILD --target=wasm-jsffi-ghc-demo:exe:console-log

hello-cf:
  DO +BUILD --target=cloudflare-worker:exe:hello-worker
