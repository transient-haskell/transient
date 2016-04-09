![](https://raw.githubusercontent.com/agocorona/transient/master/logo.ico)

See the [Wiki](https://github.com/agocorona/transient/wiki)

transient-universe is the distributed computing extension of transient.  It support moving computations (Haskell closures) from a computer in the network to another even among different architectures:  Linux nodes can work with windows and browser nodes running haskell compiled with [ghcjs](https://github.com/ghcjs/ghcjs).

The primitives that perform the moving of computations are called `wormhole` and `teleport`, the names expresses the semantic. Hence the name of the package.

All the nodes run the same program compiled for different architectures. It defines a Cloud way of computing (monad). It is a thin layer on top of transient with additional primitives and services that run a single program in one or many nodes.

Browser integration
==================

Browser nodes, running transient programs compiled with ghcjs are integrated with server nodes, using websockets communications. Just compile the program with ghcjs and point the browser to http://server:port. The server nodes have a HTTP server that will send the compiled program to the browser.

Distributed Browser/server Widgets
-------
Browser nodes can integrate Hplayground for ghcjs, a reactive client side library based in trasient (package ghcjs-hplay) they can create widgets with HTML form elements and control the server nodes. A computation can move from browser to server and back at runtime despite the different architecture.

Widgets with code running in browser and servers can compose with other widgets. A Browser node can access to many server nodes

Map-reduce
==========
transient-universe implements map-reduce in the style of [spark](http://spark.apache.org) as a particular case. It is at the same time a hard test of the distributed primitives since it involves a complex choreography of movement of computations. It supports in memory operations and caching. resilience (restart from the last checkpoint in case of failure) is not implemented but it is foreseen.

Look at [this article](https://www.schoolofhaskell.com/user/agocorona/estimation-of-using-distributed-computing-streaming-transient-effects-vi-1#distributed-datasets)

Currently I'm profiling to make map-reduce more efficient. It is not yet ready for serous data analysis.

General distributed primitives
=============================
`teleport` is a  primitive that translates computations back and forth reusing an already opened connection.

The connection is initiated by `wormhole`  with another node. This can be done anywhere in a computation without breaking composability. As always, Everything is composable.

both primitives support also streaming among nodes in an efficient way. It means that a remote call  can return not a single response but many of them.

All the other distributed primitives: `runAt`, `streamFrom` `clustered` etc are rewritten in terms of these two.

How to run the ghcjs example:
=============================
You need ghc and ghcjs installed.

clone and install perch:

    > git clone https://github.com/geraldus/ghcjs-perch
    > cd ghcjs-perch
    > cabal install --ghcjs -f ghcjs

clone and install  transient:

    > git clone https://github.com/agocorona/transient
    > cd transient
    > cabal install
    > cabal install --ghcjs

clone and install hplay:

    > git clone https://github.com/agocorona/ghcjs-hplay
    > cd ghcjs-hplay
    > cabal install
    > cabal install --ghcjs -f ghcjs

clone and install  transient-universe:

    > git clone https://github.com/agocorona/transient-universe
    > cd transient-universe
    > cabal install
    > cabal install --ghcjs

for fast development interactions, use the script

    > buildrun examples/webapp.hs

This will compile examples/webapp.hs for ghcjs and run it interpreted with runghc


then point a browser to: http:localhost:2020

See this [video](https://www.livecoding.tv/agocorona/videos/Ke1Qz-seamless-composable-web-programming) to see this example running:

The test program run among other things, two copies of a widget that start, stop and display a counter that run in the server.


Future plans
============
The only way to improve it is using it. Please send me bugs and additional functionalities!

-I plan to improve map-reduce to create a viable platform for serious data analysis and machine learning using haskell. It will have a  web notebook running in the browser.

-Create services and examples for general Web applications with distributed servers and create services for them





