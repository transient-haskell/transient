ghcjs -isrc -i../ghcjs-hplay/src  %1 -o static/out
if %errorlevel% neq 0 exit
runghc -isrc -i../ghcjs-hplay/src %1

