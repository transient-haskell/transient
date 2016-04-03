#!/bin/bash

set -e
ghcjs -isrc -i../ghcjs-hplay/src  $1 -o static/out
runghc -isrc -i../ghcjs-hplay/src $1
