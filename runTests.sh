#!/bin/sh

function showUsage()
{
   echo "Usage: $1 [ build | rebuild | run ]"
}

if [ -z "$1" ]; then
   showUsage $0
   exit 1;
fi

if [ "$1" = "run" ]; then
    cabal test
elif [ "$1" = "build" ]; then
    cabal configure --enable-tests && cabal build && cabal test
elif [ "$1" = "rebuild" ]; then
    cabal clean
    cabal configure --enable-tests && cabal build && cabal test
else
    showUsage $0
fi
