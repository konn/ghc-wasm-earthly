FROM simonwhitaker/gibo AS gibo

FROM --platform=linux/amd64 mcr.microsoft.com/devcontainers/base:jammy

RUN apt-get update && \
    apt-get -y install --no-install-recommends git sudo jq bc make automake rsync htop curl build-essential lsb-release pkg-config libffi-dev libgmp-dev software-properties-common libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ wget libncursesw5 libtool autoconf zstd && \
    apt-get clean

COPY --from=gibo /gibo /usr/local/bin/gibo

USER vscode

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin

RUN ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-prereleases-0.0.8.yaml
RUN ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.8.yaml
RUN ghcup install cabal 3.11.0.0.2024.4.19

# Sets-up GHC WASM32 backend
RUN mkdir -p /home/vscode/work-wasm
WORKDIR /home/vscode
RUN git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git

WORKDIR /home/vscode/ghc-wasm-meta
RUN bash setup.sh
RUN . ~/.ghc-wasm/env && \
  ghcup install ghc wasm32-wasi-9.10.0.20240412 -- \
  --host=x86_64-linux --with-intree-gmp --with-system-libffi

# Installs corresponding (ordinary GHC 9.10)

WORKDIR /home/vscode
RUN rm -rf /home/vscode/work-wasm

RUN echo ". ~/.ghc-wasm/env" >> /home/vscode/.bashrc
RUN echo "source ~/.ghc-wasm/env" >> /home/vscode/.zprofile
RUN echo "source ~/.ghc-wasm/env" >> /home/vscode/.zshrc
