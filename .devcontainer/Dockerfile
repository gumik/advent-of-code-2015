FROM ubuntu:22.04

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libnuma-dev libncurses-dev libncurses5 libtinfo5 wget git
RUN wget https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup -O /usr/local/bin/ghcup
RUN chmod +x /usr/local/bin/ghcup
RUN ghcup install ghc 9.4.7
RUN ghcup install cabal 3.6.2.0
RUN ghcup install stack 2.11.1
RUN ghcup install hls 2.4.0.0

RUN echo export PATH="$PATH:/root/.ghcup/bin" >> /root/.bashrc
RUN ghcup set ghc 9.4.7
RUN /root/.ghcup/bin/stack config set system-ghc --global true
