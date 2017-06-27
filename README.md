# Malodivo

## Development

To install a build environment for development and testing:

    cabal sandbox init
    cabal install --only-dependencies

Test run:

    cabal run <input.json

Currently outputs a dummy result without doing any actual calculations.

