ld:
cabal sandbox init                   # Initialise the sandbox
cabal install --only-dependencies    # Install dependencies into the sandbox
cabal build                          # Build your package inside the sandbox

cabal configure
cabal build

To run tests:

> cabal repl
> verify
