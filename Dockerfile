FROM haskell:7.10

WORKDIR /opt/gho

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./gho.cabal /opt/gho/

# Docker will cache this command as a layer, freeing us up to
# modify source code without reRUNinstalling dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/gho/
RUN cabal install

RUN cabal install happy
RUN cabal install MissingH
RUN cabal install directory
RUN cabal install
RUN cabal build
RUN cabal configure
RUN cabal build
RUN cabal install --enable-tests --only-dependencies

CMD ["cabal test"]
