cd /ghcjs-hplay
git pull
cd ../transient
git pull
chmod 777 buildrun.sh
ghcjs -isrc -i../ghcjs-hplay/src tests/Test
runghc -isrc -i../ghcjs-hplay/src tests/Test

