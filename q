A lot of things:

browser nodes running transient programs compiled with ghcjs are
integrated with server nodes. Just compile the program with ghcjs and
point the browser to http://server:port.  

Browser nodes integrate Hplayground (package ghcjs-hplay) they can create
widgets and control the server nodes. A computation can move from browser
to server and back.  Widgets with code running in browser and servers can
compose with others. browser nodes can control many server nodes.

map-reduce (Transient.DDS module) now has a true shuffle stage. Not tested
yet.

EVERITHING NEW IS IN AN ALPHA STAGE.

# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
# On branch ghcjs
# Changes to be committed:
#	new file:   buildrun.bat
#	new file:   buildrun.sh
#	modified:   examples/MainSamples.hs
#	modified:   examples/PiDistribCountinuous.hs
#	modified:   src/Transient/Backtrack.hs
#	modified:   src/Transient/Base.hs
#	modified:   src/Transient/DDS.hs
#	modified:   src/Transient/EVars.hs
#	modified:   src/Transient/Indeterminism.hs
#	modified:   src/Transient/Logged.hs
#	modified:   src/Transient/Move.hs
#	modified:   src/Transient/Move/Services.hs
#	modified:   src/Transient/Stream/Resource.hs
#	modified:   tests/Test.hs
#	new file:   tests/ghcjs-websockets.hs
#	new file:   tests/test4.hs
#	modified:   transient.cabal
#	new file:   transient.lkshf
#
# Changes not staged for commit:
#	modified:   .gitignore
#
