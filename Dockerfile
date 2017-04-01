FROM haskell:7.10

WORKDIR /opt/gho


RUN cabal sandbox init
RUN cabal update
RUN cabal --version
RUN ghc --version

# Add just the .cabal file to capture dependencies
COPY ./gho.cabal /opt/gho/

# Docker will cache this command as a layer, freeing us up to
# modify source code without reinstalling dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY gho.cabal /opt/gho/
COPY src /opt/gho/src
RUN cabal install --enable-tests --only-dependencies
RUN cabal install
COPY test /opt/gho/test
RUN cabal test

