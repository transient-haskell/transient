transient
=========

Functional reactive programming has no notion of event scope. A functional, declarative, reactive computation is affected as a whole by a signal, and must re-start from the beginning in some way, since it is declarative. A monad can contain the scope of the signal, so part of the computation already done, upstream, do not change when a signal is injected at some level of the computation by an active component.

A monad can decompose the computation in a chain of event handlers virtually set up by the monadic computation when it is called by the top signal, that is, when it is run for the first time.

This has many applications. Imagine this computation:

    profits= do
      quantity ←  waitEvent "quantity"
      liftIO $ do
            putStr "quantity="
            print quantity
            getChar
      price ←  waitEvent "price"
      liftIO $ do
            putStr "price="
            print price
            getChar
      liftIO $ do
         putStr $ "total="
         print $ quantity * price
         getChar
      return $ quantity * price


Suppose that quantity and price changes along the day, so there are two events, one when the stock sold ("quantity") changes and one when price changes. Suppose that, instead of printing in the console, the process update a display in an executive dashboard. Suppose that the computation is much more complicated and costly with access to databases and OLAP reports, with a lot of different events. We need only that when the price changes, the monad execute just the code needed to change the price and the profits without touching the other costly branches of the monad, including not only the part of the computation that is above, upstream, but also some branches that may be after the event, if we wish.

the name `Transient` comes from my old idea of closures in a monadic expression as transient pure states that are only reevaluated when some change appears upstream.

See A monad for reactive programming:

https://www.fpcomplete.com/user/agocorona/a-monad-for-reactive-programming-part-1



