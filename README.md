transient
=========

One of the biggest dreams of software is total composability.

This may be put in these terms: 

let `ap1` and `ap2` two applications with arbitrary complexity, with all effects including multiple threads, asynchronous IO, indeterminism, events and perhaps, distributed computing.

then the combinations:

     - ap1 <|> ap2  
     - ap1 >>= \x -> ap2 
     - ap1 <> ap2 
     - (,) <$> ap1 <*> ap2
     
are possible and generate a new applications that are composable as well.
     
Transient does exactly that.

For this purpose transient is a extensible effects monad with all major effects and
primitives for parallelism, events, asyncronous IO, early termination, non-determinism
logging and distributed computing. Since it is possible to extend it with more effects without
adding monad transformers, the composability is assured.

See the articles:

- [Philosophy, async, parallelism, thread control, events, Session state](https://www.fpcomplete.com/user/agocorona/EDSL-for-hard-working-IT-programmers?show=tutorials)
- [Backtracking and undoing IO transactions](https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions?show=tutorials)
- [Non-deterministic list like processing, multithreading](https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii?show=tutorials)
- [Distributed computing](https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv?show=tutorials)

These articles contains executable examples

This is the text of the first article:

# Introduction #

I have a problem: How I present the few applicative and monadic combinators that I just developed. I could present it as 

- something for  multithreaded event handling without inversion of control. Or 
- something for paralelization of processes: [async](https://hackage.haskell.org/package/async) without the wait
- for automatic thread control
- for alternative and applicative composition of parallel IO actions
- for indeterminism and asynchronicity effects 
- for high level programming at the specification level 
- for creating and  composing applications by means of a single expression
- for overcoming futures and promises of Scala and JavaScript making them unnecessary 


Too much stuff for a single article. Maybe I should split this article and taking time to write something more extensive and less dense. But I'm lazy and moreover they are only a few primitives, four or five, and no new operators. Breaking the article would hide the big picture. It would not display the beautiful unity of the common solution. 

- If you are interested in how the idea came about read the next paragrap
- If you are interested in the internals, read the section "Enter the monad"
- If you are interested in how the monad control multiple threads see "Implicit thread control"
- If you are interested in examples read  from "Example" on
- If you are interested in Async, promises and futures, read "Beyond futures and promises"
- If you are interested in running the examples read "Composition of Programs"
- If you are interested in de-inverting the control of callbacks see "deinverting callbacks"


#The problem: parallelization, concurrency and inversion of control#

Suppose that I have a blocking computation that return data when something happens. It may be also a long running computation that blocks the thread for a time:


``` haskell
     receive: IO a
```

I can use it as such, but it blocks. I can not use it in a context where other events are firing. I must create a thread for each blocking IO call. All of these threads probably modify a central state. Otherwise there will be no communication of data among threads. Alternatively someone may have created a kind of framework for this particular problem, where these blocking calls are managed. It may be a GUI toolkit, or a Web application framework, A browser environment or a library for the management of an ATM machine etc. in any case, What the programmer see  case is a set of blocking synchronous calls and a set of callbacks or handlers that he has to program and configure in the  framework. 

Blocking IO creates the need to resort to manual thread management and concurrency. That means that the code is split into parallel and concurrent chunks which are hard to code and debug. In the second case I have non blocking IO, since the thread management is done implicitly by the framework, but, in the other side, I have to split the program logic into disconnected chunks. As a result, the program logic is very hard to grasp. This is know as the callback hell, a consequence of the inversion of control.

#The OOP non-solution#

The second scenario appears when threading is managed by a framework. Essentially it is the same case. In both cases we end up with disconnected chunks of code and a mutable state. The standard way to manage this messy central state has been to divide it into smaller states and encapsulate them  with the methods that modify and serve the state.  This is the Object Oriented Programming solution; The first OOP languages were created for managing events (SIMULA), and mouse events in a interactive GUI (Smalltalk)

*Object-oriented programming is an exceptionally bad idea which could only have originated in California. - Dijkstra*

Object Oriented Programming naturally fit with this inversion of control, that pervades IT problems. Whenever there are more than one asynchronous input, there is multitasking or inversion of control with state. The solution is a state machine. What OOP does is to split this state machine into smaller state machines called objects, that interact among them. But that implies the need to "deconstruct" the specifications. 

##Deconstruct the specification recipe considered harmful##
Usually the specification of something is naturally  expressed as if this "something" is a process, in the third person active perspective. People describe things that are the focus of their analysis as active, and the environment is described as passive even if there are  active parts is the environment. In a recipe they say : you "fry the eggs" not " there is fire and there is eggs, you start the fire and the eggs are fried by the fire". You see that the passive perspective implies to give protagonism to low level elements that you are not interested in when writing an specification. Creating an OOP solution implies the deconstruction of the specification recipe into  multiple third person passive perspectives, one for each class or object.

In any case, the programs are made of disconnected pieces, the software does not follow the natural flow as it would naturally extracted from the specifications and has many low level details, so the resulting code is low level, it is hard to maintain and the final service created is not composable. There is no way to insert your service within something bigger with a single invocation. This derives in the scarce reusability of the software, and the need of profuse documentation. It is inelegant, buggy, hard to maintain, and permits an huge number of arbitrary alternatives in the design space that aggravates the mentioned problems. It may be though that this is good for the IT business, but not in the medium-long term. And moreover, there are better ways to do it.

Functional languages help little on that if people limit themselves to porting already existing solutions from OOP languages. 

#What we need #
*Simplicity is prerequisite for reliability. - Dijkstra*

The application must be programmed following the natural flow defined in the specification. The code must not split the specifications into parallel running tasks, neither invert the control and deconstruct the specification into objects. The design space must be limited so everyone should program the same specification the same way. So other's code can be grasped immediately without the aid of external documentation. The application must transport user-defined state, that can be inspected and updated, added and deleted, but this state is instrumental it is not the center, because the center is the process described in the specification.

**We need an EDSL for hardworking IT programmers, that use Java, JavaScript, Scala, C#, PHP, Ruby or Python and don't know Haskell. Not a monad stack but a simple Monad that may liberate them from the Oppressive Object Paradigm, or OOP, without forcing them to sacrifice to the gods of Category Theory. With applicative and alternative combinators and a few primitives for implicit parallelization and thread control and for de-inversion of callbacks in the IO monad. Plus user-defined state management and early termination**

*“Simplicity is a great virtue but it requires hard work to achieve it and education to appreciate it. And to make matters worse: complexity sells. - Dijkstra*

#Enter the monad#
A monad with the asynchronicity effect can rescue the I.T industry from the inversion of control trap for which OOP was originally designed while allowing implicit parallelization and thread control. A entire application can be coded in a single monadic expression with little or no plumbing. That allows the creation of composable applications and services of the [A -> A -> A](http://www.haskellforall.com/2014/04/scalable-program-architectures.html) kind. 


In [A monad for reactive programming](https://www.fpcomplete.com/user/agocorona/monad-reactive-programming-2) I defined a monad that de-invert the control when there are different events. The `Transient` monad can listen for events at different points in the monadic expression. It is no longer necessary to have a single listen point like in the case of the event loop or in classical reactive frameworks. Moreover, these events listeners do not block, so every event watchig point is active in the monad at the same time.

The events in the above article are injected by  a simulated event loop in the state monad. This time I will show how to listen for IO computations without the help of a framework that bring events. These events may be hardware buttons, device driver inputs, requests from users, responses from databases, requests from other systems in the cloud etc.   

What we intend here is to formulate a general solution that permit coding close to the user requirement document and presenting the application as a single process even if involves multiple inputs and parallel executions, in a single monadic expression. This expression will spawn communicate and kill tasks whenever necessary automatically. We will see that we can  improve the readability and reduce the complexity, so we can increase the maintainability and making entire services or applications composable.

Since I have to deal with dirty things like blocking, threads and IO, don't expect what follows to be a walk in the Platonic realm. I start with a monad like the `Transient` monad, that can be stopped with `empty`  and continued with `runCont cont` where `cont` is the continuation context, set with `getCont`.  (explanation below)

``` haskell
    data Transient m x= Transient  {runTrans :: m (Maybe x)}

    data EventF  = forall a b . EventF
             {xcomp :: (TransientIO a)
             ,fcomp :: [a -> TransientIO b]
             , ... other ....}

    type StateIO= StateT EventF  IO

    type TransientIO= Transient StateIO
    
    instance Monad TransientIO where
        return x = Transient $ return $ Just x
        x >>= f = Transient $ do
            cont <- setEventCont x  f
            mk <- runTrans x
            resetEventCont cont
            case mk of
                Just k  -> runTrans $ f k
                Nothing -> return Nothing

    instance Applicative TransientIO where
    pure a  = Transient  .  return $ Just a
    Transient f <*> Transient g= Transient $ do
        k <- f
        x <- g
        return $  k <*> x

    instance  Alternative TransientIO where
        empty= Transient $ return  Nothing
        Transient f <|> Transient g= Transient $ do
            k <- f
            x <- g
            return $  k <|> x

    getCont ::(MonadState EventF  m) => m EventF
    getCont = get

    runCont :: EventF -> StateIO ()
    runCont (EventF  x fs ...)= do runIt x (unsafeCoerce fs); return ()
      where
      runIt x fs= runTrans $  x >>= compose fs

      compose []= const empty
      compose (f: fs)= \x -> f x >>= compose fs
```

For a view of how this monad has evolved look at the first article [A monad for reactive programming part 1](https://www.fpcomplete.com/user/agocorona/a-monad-for-reactive-programming-part-1) where I present a simpler version of this monad that has some shortcomings. In the second part I solved these shortcomings. I think that this is the best way to understand it.

What this monad does is to store the closure `x` and the continuations `f` in the state. `getCont` captures the execution state at the point and `runCont` executes it. 

As far as "continuation" is taken here, there may be more than one of them.

For example, in this expression:

     x0 >>=((x >>= f1) >>= f2) >>= f3
   
for the closure generated at the execution point `x`, the continuations are  

     f1 >>= f2 >>= f3

And the closure is the result of the execution of `x0 >>= x`

What `setEventCont` and `resetEventCont` does is to compose the list of continuations (one for each nested expression) in a 'flattened' representation, as a list in `fcomp`. Since the list does not "know" that the continuations types match, I have to erase the types using `unsafeCoerce`.

# Parallelization #
With these three primitives `getCont` `runCont` and `empty` I will define a `async` primitive that will run a blocking IO action in a new thread and will execute the continuation in that thread when in receives something:

``` haskell
    buffer :: Dynamic 
    
    buffer= unsafePerformIO $ newEmptyMVar

    async :: IO a -> TransientIO a
    async receive =  do
      cont <- getCont
      r <- liftIO $ tryTakeMVar buffer

      case r of

        Nothing ->do
           liftIO . forkIO $  do
              r <- receive
              putMVar buffer $ toDync  r
              runCont cont
              return()
           empty

        Just r -> return $ formDynamic r
```
 

Essentially, `async` get the continuation, then inspect the buffer. If there is Nothing then spawn `receive` in a new thread. The current thread is finished (`empty`). When something arrives, it is put in the buffer, then `runCont` will continue at the beginning of receive' in the new thread. It does so because `getCont` got the `Transient` continuation there. This time, there will be something in the buffer and will return it, so the procedure will continue after the event arrives, but in a new thread. 

Note that `receive` only fill the buffer. when `runCont`executes the closure it will inspect the buffer again. This time there will be something, the closure will succeed and the continuation will fire.

`getCont` and `runCont` are similar to [setjmp and longjmp](http://en.wikipedia.org/wiki/Setjmp.h) in C. Moreover, the mechanism is not very different form how the IO scheduler in GHC or in any operating system.  But this time it runs at the application level rather than at the GHC level.

#Wait for events#
If we want to trigger the continuation repeatedly whenever something is received by `receive`, it is a matter of adding a loop to the `Nothing` branch. Then the continuation will be called for every received event.


let's call this variant `waitEvents`:

``` haskell
    waitEvents :: IO a -> TransientIO a
    waitEvents receive =  do
      cont <- getCont
      r <- tryTakeMVar buffer

      case r of

        Nothing ->do
           liftIO . forkIO $ loop $ do
              r <- receive
              putMVar buffer  r
              runCont cont
              return()
           empty

        Just r -> return r
        
      where
      loop x= x >> loop x
```
##Example##
This program will say hello to every name entered.

``` haskell
    runTransient :: TransientIO x -> IO (Maybe x, EventF)
    runTransient t= runStateT (runTrans t) eventf0
    
    main= do
        runTransient $ do
          name <- waitEvents getLine
          liftIO $ putStrLn $ "hello "++ name
        stay
```

Note that there is no loop. waitEvent install getLine at the start of a process that execute the continuation, what is after getLine, for each entry. the loop is internal to `waitEvents`

Here `runTransient` execute a transient computation down to the IO monad.

`stay` is whatever that keep the console application from exiting. That is because since the transient branch that wait for events is non-blocking, it would finish immediately. **After async or waitEvents, the current thread dies and the rest of the monadic computation run in a different thread**

#Implicit thread control#

Since each event in any part of the monadic computation are active and trigger the continuation of the monad at that point, the monadic expression is multithreaded and non determinist. 

How to control the threads?. It is natural to think that since `waitEvents` and `async` execute continuations within the monadic expression, then once something happens  in a  statement then their continuations must be invalidated. 

That means that whenever `async` of `waitEvents` receive something, the threads that are running below must be killed. Then this statement, with the new buffered iput will execute his closure and rebuild the continuation again.

This is the natural thread management that I implemented. I do not detail the modifications necessary for `waitEvents` to permit this behaviour. It is a matter of keeping in the state the list of spawned threads so that each `waitEvents` has the information about all the threads that are triggered after it. Additionally, this list contain also a buffer for each of these threads.

In this example:

``` haskell
    main= do
        runTransient $ do
          waitEvents watchReset <|> return ()
          name <- waitEvents getLine
          liftIO $ putStrLn $ "hello "++ name 
        stay
```

The `return()` composed with the alternative operator `<|>`would bypass inmediately the wait for the reset event, but as soon as the reset is pressed, all the event handlers spawned after it will be killed. Immediately they will be spawned again.

This is a slightly different version:

    main= do
        runTransient $ do
          r <- (waitEvents watchStop >> return True) <|> return False
          if r then liftIO $ putStrln "STOP" else do
             name <- waitEvents getLine
             liftIO $ putStrLn $ "hello "++ name 
        stay

In this case the program will be stopped and will not be re-spawned when `watchStop` is activated. since now the branch of the monad executed is different. It prints the stop message and finalizes.


#Non blocking IO#
Let's create one nonblocking keyboard input thing called `option`. A the same time this is a good example of inter-thread communication within the `Transient` monad:

``` haskell
    option :: (Typeable a, Show a, Read a, Eq a) =>
               a -> [Char] -> TransientIO a
    option ret message= do
      liftIO $ putStrLn $ message++"("++show ret++")"
      waitEvents "" getLine'    
      where
      getLine'=  do
       atomically $ do
         mr <- readTVar getLineRef
         case mr of
           Nothing -> retry
           Just r -> 
              case readsPrec 0 r of
              [] -> retry
              (s,_):_ -> if ret== s
                     then do
                       writeTVar  getLineRef Nothing
                       return ret               
                     else retry
               _ -> retry

    getLineRef= unsafePerformIO $ newTVarIO Nothing

    inputLoop :: IO ()
    inputLoop=  do
           r<- getLine         !> "started inputLoop"
           if r=="end" return True else do
              atomically . writeTVar  getLineRef $ Just r
              inputLoop
```

# Applicative and Alternative combinators#

`option` read in nonblocking mode the standard input, so many options can  be combined using applicative or alternative operators. `option` shows a message and wait for `inputLoop` to enter a input line. If some `option` match, it return the value. If it does not match, it fails with `empty`, but the loop in `waitEvents` re-executes `getLine'` again for this option.  In this way, the options are continuosly watching the input. Note that more than one option can be triggered simultaneously, in a different thread.

`inputLoop` is initialized by `async`. It wait for input, and expose it to all the running `getLine'` processes (one per `option`) in a `TVar`. if the user press "end" `inputLoop` return and `async` kill all the watching threads below.

``` haskell
    main= do
       runTransient choose
       stay

    choose :: TransientIO()
    choose= do
       r <- async inputLoop <|> return False
       case r of
         True -> return ()
         False-> do
           r <-  option (1 :: Int) "red"  <|> option 2 "green" <|> option 3 "blue" 
           liftIO $ print r
```

The above program will print repeatedly  the option chosen. We see that `option` is composable using the alternative operator.

Now let's create another event generator, a number is sent every second, while two options are waiting for keyboard input:

``` haskell
    data Option= Option String  String | Number Int deriving Show
    
    choose= do
       r <-  ( Option <$> ( option "1" "red"  <*> option "2" "green")) 
         <|> ( Number <$> waitEvents  waitnumber )
       liftIO $ putStrLn $ "result=" ++ show r
       
    where
    waitnumber= do
      threadDelay 1000000
      return 42 
```

Applicative and alternative combinators can be used fully. The Applicative wait for both events to be triggered to have data in their respective buffers. `waitnumber` produce an event each second.

Each Option run a different waitEvent in a different thread, but the closure is the same applicative expression in the three threads. The three have a TVar waiting for new input. they fill their respective buffers when they validate. The thread that fill the last buffer empty them and execute the continuation. The other two fail, but stay ready for the next input, since `waitEvents`has a loop.


#Beyond futures and promises#

Scala Futures and the haskell library async manage placeholders that receive the result and place them where the result of the computation is needed. 

Scala Futures also uses futures in  nice chains of multi threaded lists that can be transformed in the style of map-reduce. 

In this sense they are similar to the javaScript promises, which chain code with `then`, but the latter does not perform multiple tasks like in the case of Scala futures.

For some needs Scala and JavaScript must use callbacks since the constraints of their frameworks do not allow enough flexibility. futures and promises forces the programmer to enter in a different kind of computation model, different from the one of the native languages. In the case of Scala is monoidal. in the case of Javascript is a restricted form of bind operation.

This library put the continuation code at the end of the receiving pipeline and parallelize the execution, but the continuation is the plain monadic code that is after the receive call in the monadic expression, so there is no restriction about what can be done. 

`asyn` can be used for any process that we want to parallelize. Now we can do it better than the [async](https://hackage.haskell.org/package/async-2.0.2/docs/Control-Concurrent-Async.html) library.  This program sum the words in google and haskell homepages **in parallel**. Using [Network.HTTP](http://hackage.haskell.org/package/HTTP-4000.2.19/docs/Network-HTTP.html)

``` haskell
    sum= do
       (r,r') <- (,) <$> async  (worker "http://www.haskell.org/")
                     <*> async  (worker "http://www.google.com/")

       liftIO $ putStrLn $ "result="  ++ show r + r'


    where
    getURL= simpleHTTP . getRequest

    worker :: String -> IO Int
    worker url=do
      r <- getURL url
      body <- getResponseBody r
      return . length . words $ body
```

That is a complete working example.  Note that unlike in the async library, there is no `wait` primitive. All the processing is done in the `worker` in his thread. When both workers finish, they return the result to the monad and continue the thread.

We also can do parallel IO processing in the style of futures of the Scala language using the `Monoid` instance of `TransientIO`. But this time since we use continuations, futures are no longer necessary: the last download trigger the continuation.

``` haskell
    instance Monoid a => Monoid (TransientIO a) where
      mappend x y = mappend <$> x <*> y  
      mempty= return mempty

    sum= do
       rs <- foldl (<>) (return 0) $ map (async . worker) 
                  [ "http://www.haskell.org/", "http://www.google.com/"]

       liftIO $ putStrLn $ "result="  ++ show rs
```

Since the `worker`return an `Int`, to sum the results we need a `Monoid` instance for `Int`

``` haskell    
    instance Monoid Int where
      mappend= (+)
      mempty= 0
```

Note that there is a full de-inversion of control, since the result return to the monad. The Futures and promises of Scala (or in javascript) can not return the execution flow to the calling procedure. Since the Transient monad continue in the thread that completed the worker, the processing can continue in the monad. That permits more complex and yet clearer computations. It is not reduced to list-like processing.

# A Web Server #
Here is a Web Server:

``` haskell

    server=  do 
       sock <-  liftIO $  listenOn $ PortNumber 80
       (h,_,_) <- spawn $ accept sock
       liftIO $ do
           hPutStr h msg
           hFlush h
           hClose h

    msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
```

In the current code, the primitives `async`, `waitEvents` and `spawn` are defined in terms of `parallel`, which is a generalization of `async` and `waitEvents` explained above :

``` haskell
    data Loop= Once | Loop | Multithread

    waitEvents ::  IO b -> TransientIO b
    waitEvents= parallel Loop

    async  :: IO b -> TransientIO b
    async = parallel Once

    spawn= parallel Multithread

    parallel  ::  Loop ->  IO b -> TransientIO b
```

When `parallel` is called with `Multithread`,  it spawn the continuation in a thread for each event received immediately without waiting for the termination of the previous one. `waitEvent` execute the continuation within the thread so the receive method is not called again until the previous event is processed.

# Composition of programs (Runnable example) #

We can compose any of these programs togeter since none of them block and the automatic thread control  apply gracefully to all of the elements. This program combines the above programs and some others.

The combination in this case is using the alternative operator:

    colors <|> app  <|> sum1 <|> sum2 <|> server <|> menu

Since the fpcomplete environment uses ghci and it shares threads among snippets of code, I can run only one example in this article, and the composability of Transient is nice to show them all togeter.

To verify the multitasking press: app  and then colors.  app would start an iterative counter with an applicative expression, while colors will ask for an option among tree of them. both will run in parallel until you press "main" which will stop both, since main is above in the monad.

Code running at the original article:

     https://www.fpcomplete.com/tutorial-edit/An%20EDSL%20for%20hard%20working%20IT%20programmers

#Session data#

I added a type indexed map to the state so the user can store his own session data with these primitives:

``` haskell
    setSData ::  ( MonadState EventF m,Typeable a) => a -> m ()

    getSData :: MonadState EventF m => Typeable a =>Transient m  a
```

Session data can be used instead of a state monad transformer for each new kind of user data.

My purpose is to create a monad for general IT purposes, for profane programmers with no knowledge of monad transformers. 

Since `empty`  stop the computation but does not transport any error condition, session data can be used for this purpose:

``` haskell
    data Status= Error String | NoError

    fail :: String -> TransientIO a
    fail msg= setSData (Error msg) >> empty
```

After the execution, I can inspect the status:

``` haskell
    status <- getSData <|> NoError 
```

The alternative expression is necessary since if Status has not been set, the computation would stop. `NoError` guarantee that it does not stop.

#de-inverting callbacks#

So far so good. But what happens when besides dealing with raw blocking IO there is a framework that deal with some particular events, so it initiates the threads himself and expect you just to set the callbacks?

Suppose that we have this event handling setter:

``` haskell
     setHandlerForWhatever :: (a -> IO ()) -> IO ()
```

It is necessary a de-inversion call `whateverHappened` at some point of the computation may be at the beginning) so that the callback continues the monadic execution: 

``` haskell
     do
        somethingToDo
        r <- whatheverHappened
        doSomethingWith r
        ....
```

To define the de-inverted call `whateverHappened` we use the same trick than in async, but this time there is no `forkIO` neither thread control, since the framework does it for you:

``` haskell
     whateverHappened= do
        cont <- getCont
        mEvData <- Just <*> getSData <|> return Nothing
        case mEvData of
          Nothing ->  setHandlerForWhatever $\dat -> do
              runStateT ( setSData dat >> runTansient cont) cont
              empty
          Just dat -> return dat
```
Whether the framework is single threaded  or multi threaded is not important, we give it the event handlers that it need by means of continuations.

To have something more general, I defined:

``` haskell
   type EventSetter eventdata response= (eventdata ->  IO response) -> IO ()
   type ToReturn  response=  IO response
   
   react
      :: Typeable eventdata
      => EventSetter eventdata response
      -> ToReturn  response
      -> TransientIO eventdata
```

the second parameter is the value returned by the callback. So if you have a callback called `OnResponse` 

``` haskell
    data Control= SendMore | MoMore
    
    OnResponse :: (Response -> IO Control) -> IO()
```

I can display all  data received  while controlling the reception this way:

``` haskell
    rcontrol <- newMVar Control
    
    resp <- react $ OnResponse (const $ readMVar rcontrol)
    display resp
    r <- (option "more" "more" >> return SendMore) <|> (option "stop" "stop" >> return NoMore)
    putMVar rcontrol r
    
    
```
Since react set as callback all the rest of the computation and since the ToReturn expression is evaluated the latest, the continuation is executed and set rcontrol before  the `ToReturn` expression is evaluated.

Note that you can reassign the callback at any moment since react would set whatever continuation that is after it.




#Conclusions and future work#

The code is at 

    https://github.com/agocorona/transient

My aim is to create a family of combinators for programming in industry. As I said before, that implies no monad transformers, the simplest monad that could produce the simplest error messages.

The haskell applicative, alternative, monoidal and monadic combinators when applied to a monad that  manage asynchronous IO permits multithreaded programming with little plumbing that is close to the specification level with great composability. No inversion of control means no need to deconstruct the specifications and no state machines. 

This, together with the uniform and composable thread management, narrow the design space and makes the application more understandable from the requirements, and thus the technical documentation and maintenance costs are reduced to a minimum.

Note that the bulk of the programming is done in the IO monad. That is on purpose. The idea is a simple IT EDSL with the rigth effects that permit rapid and intuitive development. Additional monads can be used by running them within the IO procedures defined by the programmer if they wish. I will add some additional effects like backtracking to undo transactions and to produce execution traces. That would be the base of a new version of MFlow, my server-side framework and integration platform. The ability to perform rollbacks and respond to asynchronous events at the same time is important for cloud applications.  reader and writer  effects for any programmer need are almost trivial to implement using `getSData` and `setSData`.

Resource allocation and deallocation for file handlers etc can be done using the same strategy used for thread control, but it is more orthogonal to delegate it to the IO threads themselves. The programmer can use exceptions or monads that guarantee proper release of resources before the thread is killed.

In `Multithread` mode the single entry buffer can be overrun. It is necessary to handle this case or, else, assume that the `receive` procedure has his own buffer and his own event contention mechanism. That is the most orthogonal option.

This is huge. I plan to create interfaces for some GUI toolkit. The GUI objects will be fully composable for the first time. 

Spawning threads in other machines is the next big step. [MFlow](http://mflowdemo.herokuapp.com/) and [hplayground](http://tryplayg.herokuapp.com/) will converge with this platform. If you want to collaborate, don´t hesitate to send me a message!

With the `react` primitive It is possible to de-invert any framework, including the callbacks of a GUI toolkit, so the widgets can be managed with Applicative and monadic combinators. hplayground does that for the Javascript callbacks and HTML forms. Since hplayground, that run in the client and MFlow that run server side share the same widget EDSL, that can be ported to a GUI, an application can run in any environment, including console applications.


