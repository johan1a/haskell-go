To build:
cabal sandbox init                   # Initialise the sandbox
cabal install happy
cabal install alex
cabal install MissingH
cabal install directory
cabal install 			     # Install dependencies into the sandbox
cabal build                          # Build your package inside the sandbox

cabal configure
cabal build

To run tests:

cabal install --enable-tests --only-dependencies

cabal test
