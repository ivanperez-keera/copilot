FROM ubuntu:lunar

RUN apt-get update -y
RUN apt-get install -y cabal-install ghc
RUN apt-get install -y clang llvm
RUN apt-get install -y z3
RUN apt-get install -y git
RUN git clone https://github.com/GaloisInc/copilot-verifier
WORKDIR copilot-verifier
RUN git submodule update --init
RUN cabal update
RUN apt-get install -y libz-dev
RUN cabal test copilot-verifier
