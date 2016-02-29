#!/bin/sh

cabal configure -ftest && cabal build && cd tests && ../dist/build/test/test
