[![Build Status](https://travis-ci.org/johan1a/haskell-go.svg?branch=master)](https://travis-ci.org/johan1a/haskell-go)

# Haskell-Go

## How to build:
    cabal sandbox init                   # Initialise the sandbox
    cabal update
    cabal install happy                  # TODO necessary?
    cabal install alex
    cabal install MissingH
    cabal install directory
    cabal install 			                 # Install dependencies into the sandbox
    cabal build                          # Build your package inside the sandbox

## How to run:
    ./dist/build/gho/gho <file>.go

## How to run tests:
    cabal install --enable-tests --only-dependencies
    cabal test`
