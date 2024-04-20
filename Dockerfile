FROM simonwhitaker/gibo AS gibo

FROM --platform=linux/amd64 mcr.microsoft.com/devcontainers/base:jammy

RUN apt-get update && \
    apt-get -y install --no-install-recommends git sudo jq bc make automake rsync htop curl build-essential lsb-release pkg-config libffi-dev libgmp-dev software-properties-common libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ wget libncursesw5 libtool autoconf zstd && \
    apt-get clean

COPY --from=gibo /gibo /usr/local/bin/gibo

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

RUN ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.8.yaml
RUN ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.8.yaml
RUN ghcup install cabal 3.11.0.0.2024.4.19

# Sets-up GHC WASM32 backend
RUN mkdir -p /root/work-wasm
WORKDIR /root
RUN git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git

WORKDIR /root/ghc-wasm-meta
RUN bash setup.sh
RUN . ~/.ghc-wasm/env && \
  ghcup install ghc wasm32-wasi-9.10.0.20240412 -- \
  --host=x86_64-linux --with-intree-gmp --with-system-libffi

WORKDIR /root
RUN rm -rf /root/work-wasm

# The following doesn't work well..
# RUN echo ". /root/.ghc-wasm/env" >> /etc/profile

# So just copy-and-pasting the content of .ghc-wasm env here instead.
ENV PATH=/root/.ghc-wasm/wasm-run/bin:$PATH
ENV PATH=/root/.ghc-wasm/proot/bin:$PATH
ENV PATH=/root/.ghc-wasm/wasm32-wasi-cabal/bin:$PATH
ENV PATH=/root/.ghc-wasm/cabal/bin:$PATH
ENV PATH=/root/.ghc-wasm/wazero/bin:$PATH
ENV PATH=/root/.ghc-wasm/wasmedge/bin:$PATH
ENV PATH=/root/.ghc-wasm/wasmtime/bin:$PATH
ENV PATH=/root/.ghc-wasm/wabt/bin:$PATH
ENV PATH=/root/.ghc-wasm/binaryen/bin:$PATH
ENV PATH=/root/.ghc-wasm/bun/bin:$PATH
ENV PATH=/root/.ghc-wasm/nodejs/bin:$PATH
ENV PATH=/root/.ghc-wasm/deno/bin:$PATH
ENV PATH=/root/.ghc-wasm/wasi-sdk/bin:$PATH
ENV PATH=/root/.ghc-wasm/wasm32-wasi-ghc/bin:$PATH
ENV AR=/root/.ghc-wasm/wasi-sdk/bin/llvm-ar
ENV CC=/root/.ghc-wasm/wasi-sdk/bin/clang
ENV CC_FOR_BUILD=cc
ENV CXX=/root/.ghc-wasm/wasi-sdk/bin/clang++
ENV LD=/root/.ghc-wasm/wasi-sdk/bin/wasm-ld
ENV NM=/root/.ghc-wasm/wasi-sdk/bin/llvm-nm
ENV OBJCOPY=/root/.ghc-wasm/wasi-sdk/bin/llvm-objcopy
ENV OBJDUMP=/root/.ghc-wasm/wasi-sdk/bin/llvm-objdump
ENV RANLIB=/root/.ghc-wasm/wasi-sdk/bin/llvm-ranlib
ENV SIZE=/root/.ghc-wasm/wasi-sdk/bin/llvm-size
ENV STRINGS=/root/.ghc-wasm/wasi-sdk/bin/llvm-strings
ENV STRIP=/root/.ghc-wasm/wasi-sdk/bin/llvm-strip
ENV LLC=/bin/false
ENV OPT=/bin/false
ENV CONF_CC_OPTS_STAGE2=${CONF_CC_OPTS_STAGE2:-"-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -O3 -fno-strict-aliasing -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"}
ENV CONF_CXX_OPTS_STAGE2=${CONF_CXX_OPTS_STAGE2:-"-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -fno-exceptions -O3 -fno-strict-aliasing -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"}
ENV CONF_GCC_LINKER_OPTS_STAGE2=${CONF_GCC_LINKER_OPTS_STAGE2:-"-Wl,--compress-relocations,--error-limit=0,--growable-table,--keep-section=ghc_wasm_jsffi,--stack-first,--strip-debug "}
ENV CONF_CC_OPTS_STAGE1=${CONF_CC_OPTS_STAGE1:-"-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -O3 -fno-strict-aliasing -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"}
ENV CONF_CXX_OPTS_STAGE1=${CONF_CXX_OPTS_STAGE1:-"-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -fno-exceptions -O3 -fno-strict-aliasing -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"}
ENV CONF_GCC_LINKER_OPTS_STAGE1=${CONF_GCC_LINKER_OPTS_STAGE1:-"-Wl,--compress-relocations,--error-limit=0,--growable-table,--keep-section=ghc_wasm_jsffi,--stack-first,--strip-debug "}
ENV CONFIGURE_ARGS=${CONFIGURE_ARGS:-"--host=x86_64-linux --target=wasm32-wasi --with-intree-gmp --with-system-libffi"}
ENV CROSS_EMULATOR=${CROSS_EMULATOR:-"/root/.ghc-wasm/wasm-run/bin/wasmtime.sh"}


