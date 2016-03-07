sed -i -- "s/\"localhost\"/$1/g"  "./tests/Test.hs"
ghcjs -isrc -i../ghcjs-hplay/src -i../ghcjs-perch/src tests/Test
runghc -isrc -i../ghcjs-hplay/src -i../ghcjs-perch/src tests/Test

