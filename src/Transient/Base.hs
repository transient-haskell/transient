{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | See http://github.com/agocorona/transient
-- everithing in this module is exported in order to allow extensibility.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE Rank2Types        #-}
-- show
module Transient.Base  where
-- /show

import           Control.Applicative
import           Control.Monad.State
import           Data.Dynamic
import qualified Data.Map               as M
import           Data.Monoid
import           Debug.Trace
import           System.IO.Unsafe
import           Unsafe.Coerce
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           System.Mem.StableName
import           Data.Maybe
import           GHC.Conc
import           Data.List
import           Data.IORef
import           System.Environment

--{-# INLINE (!>) #-}
--(!>) :: Show a => b -> a -> b
--(!>) x y=   x !!> show y
--infixr 0 !>
--(!!>) =   flip trace
--infixr 0 !!>

data TransIO  x = Transient  {runTrans :: StateT EventF IO (Maybe x)}
type SData= ()

type EventId= Int

type TransientIO= TransIO

data EventF  = forall a b . EventF{effects     :: Effects
                                  ,event       :: Maybe SData
                                  ,xcomp       :: TransIO a
                                  ,fcomp       :: [b -> TransIO b]
                                  ,mfData      :: M.Map TypeRep SData
                                  ,mfSequence  :: Int
                                  ,threadId    :: ThreadId
                                  ,freeTh      :: Bool
                                  ,parent      :: Maybe EventF
                                  ,children    :: TVar[EventF]
                                  ,maxThread   :: Maybe (P Int)
                                  }
                                  deriving Typeable


type P= IORef
newp= newIORef

type Effects= forall a b c.TransIO a -> TransIO a -> (a -> TransIO b)
     -> StateIO (StateIO (Maybe c) -> StateIO (Maybe c), Maybe a)





instance MonadState EventF TransIO where
  get  = Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())
  state f =  Transient $ do
      s <- get
      let ~(a, s') = f s
      put s'
      return $ Just a

type StateIO= StateT EventF  IO

--type TransIO= Transient StateIO

--runTrans ::  TransIO x -> StateT EventF  IO (Maybe x)
--runTrans (Transient mx) = mx


runTransient :: TransIO x -> IO (Maybe x, EventF)
runTransient t= do
  th <- myThreadId
  let eventf0=  EventF baseEffects Nothing empty [] M.empty 0
          th False  Nothing  (unsafePerformIO $ newTVarIO []) Nothing


  runStateT (runTrans t) eventf0




-- | get the continuation context: closure, continuation, state, child threads etc
getCont :: TransIO EventF
getCont = Transient $ Just <$> get

-- | run the closure and the continuation context
runCont :: EventF -> StateIO (Maybe a)
runCont (EventF _ _ x fs _ _  _ _  _ _ _)= runTrans $ do
      r <- (unsafeCoerce x)
      (compose fs r)


{-
runCont cont= do
     mr <- runClosure cont
     case mr of
         Nothing -> return Nothing
         Just r -> runContinuation cont r
-}


-- | compose a list of continuations
compose []= const empty
compose (f: fs)= \x -> f x >>= compose fs



-- | run the closure  (the 'x'  in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateIO (Maybe a)
runClosure (EventF _ _ x _ _ _ _ _ _ _ _) =  unsafeCoerce $ runTrans x

-- | run the continuation (the 'f' in 'x >>= f') of the current bind operation
runContinuation ::  EventF -> a -> StateIO (Maybe b)
runContinuation (EventF _ _ _ fs _ _ _ _  _ _ _) =
   runTrans . (unsafeCoerce $ compose $  fs)


setContinuation :: TransIO a -> (a -> TransIO b) -> [c -> TransIO c] -> StateIO ()
setContinuation  b c fs= do
    (EventF eff _ _ _ d e f g h i j) <- get
    put $ EventF eff Nothing b (unsafeCoerce c:fs) d e f g h i j

-- | run a chain of continuations. It is up to the programmer to assure by construction that
--  each continuation type-check with the next and the parameter type match the input of the first
-- continuation and that the output is of the type intended.
runContinuations :: [a -> TransIO b] -> c -> TransIO d
runContinuations fs x= (compose $ unsafeCoerce fs)  x

instance   Functor TransIO where
  fmap f mx=   -- Transient $ fmap (fmap f) $ runTrans mx
    do
     x <- mx
     return $ f x

instance Applicative TransIO where
  pure a  = Transient . return $ Just a

  f <*> g = Transient $ do
         rf <- liftIO $ newIORef Nothing
         rg <- liftIO $ newIORef Nothing   -- !> "NEWIOREF"


         EventF _ _ _ fs _ _ _ _ _ _ _  <- get

         let

             appg x = Transient $  do
                   liftIO $ writeIORef rg $ Just x
                   k <- liftIO $ readIORef rf
                   return $ k <*> Just x  -- !> "RETURNED: " ++ show(isJust k)++ show(isJust x)


             appf k = Transient $  do
                   liftIO $ writeIORef rf  $ Just k
                   x<- liftIO $ readIORef rg
                   return $ Just k <*> x  --  !> "RETURNED: " ++ show(isJust k)++ show(isJust x)




         setContinuation f appf fs

         k <- runTrans f
         was <- getData `onNothing` return NoRemote
         setData NoRemote
         if was== WasRemote  || was /= WasParallel  && isNothing k
           then  do

             restoreStack fs
             return Nothing
           else do

             liftIO $ writeIORef rf  k

             mfdata <- gets mfData
             seq <- gets mfSequence

             setContinuation g appg fs

             x <- runTrans g              -- !!> "RUN g"
             liftIO $ writeIORef rg  x
             restoreStack fs
             return $ k <*> x
     where
     restoreStack fs=
       modify $ \(EventF eff _ f _ a b c d parent children g1) ->
               EventF eff Nothing f fs a b c d parent children g1



-- | dynamic serializable data for logging
data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

instance Show IDynamic where
  show (IDynamic x)= show $ show x
  show (IDyns s)= show s

instance Read IDynamic where
  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str


type Recover= Bool
type CurrentPointer= [LogElem]
type LogEntries= [LogElem]
data LogElem=   WaitRemote | Exec | Step IDynamic deriving (Read,Show)
data Log= Log Recover  CurrentPointer LogEntries deriving Typeable


instance  Alternative TransIO where
  empty = Transient $ return  Nothing
  (<|>) = mplus

--  Transient f <|> Transient g= Transient $ do
--         k <-   f
--         x <-   g
--         return $ k <|> x



data RemoteStatus=   WasRemote | WasParallel | NoRemote deriving (Typeable, Eq, Show)

instance MonadPlus TransIO where
    mzero= empty
    mplus  x y=  Transient $ do
         mx <- runTrans x     -- !!> "RUNTRANS11111"
         was <- getData `onNothing` return NoRemote
         if was== WasRemote
           then return Nothing   -- !!> "WASREMOTE"
           else case mx of
             Nothing -> runTrans y     -- !!> "RUNTRANS22222"
             justx -> return justx

-- | a sinonym of empty that can be used in a monadic expression. it stop the
-- computation
stop :: TransIO a
stop= Control.Applicative.empty

infixr 1  <**  ,  <***

-- | forces the execution of the second operand even if the first fails. Return the first result
(<**) ma mb= Transient $ do
              EventF _ _ _ fs _ _ _ _ _ _ _  <- get
              setContinuation ma (\x -> mb >> return x)  fs
              rest
          where
          rest= do
              a <- runTrans ma
              b <- runTrans mb
              return $ a <* b

-- | forces the execution of the second operand  if the first fails only if the first operand
-- is executed normally, that is , it is not a reexecution consequence of an internal event on it.
-- Return the first result
(<***) ma mb= Transient $ do

              a <- runTrans ma
              b <- runTrans mb
              return $ a <* b

instance Monoid a => Monoid (TransIO a) where
  mappend x y = mappend <$> x <*> y
  mempty= return mempty

-- | set the current closure and continuation for the current statement
setEventCont ::   TransIO a -> (a -> TransIO b) -> StateIO EventF
setEventCont x f  = do

   st@(EventF eff e _ fs d n  r applic  ch rc bs)  <- get
   let cont=  EventF eff e x ( unsafeCoerce f : fs) d n  r applic  ch rc bs
   put cont
   return cont

-- | reset the closure and continuation. remove inner binds than the previous computations may have stacked
-- in the list of continuations.
--resetEventCont :: Maybe a -> EventF -> StateIO (TransIO b -> TransIO b)
resetEventCont mx _=do
   st@(EventF eff e _ fs d n  r nr  ch rc bs)  <- get
   let f= \mx ->  case mx of
                       Nothing -> empty
                       Just x  -> (unsafeCoerce $ head fs)  x
   put $ EventF eff e (f mx) ( tailsafe fs) d n  r nr  ch rc bs
   return  id

tailsafe []=[]
tailsafe (x:xs)= xs

--refEventCont= unsafePerformIO $ newIORef baseEffects

baseEffects :: Effects

baseEffects x  x' f' = do
            c <-setEventCont x'  f'
            mk <- runTrans x
            t <- resetEventCont mk c
            return (t,mk)

instance Monad TransIO where

      return x = Transient $ return $ Just x

      x >>= f  = Transient $ do
            effects <- gets effects -- liftIO $ readIORef refEventCont
            (t,mk) <- effects x x f
            t $ case mk of
                 Just k  ->  runTrans (f k)

                 Nothing ->  return Nothing

--instance MonadTrans (Transient ) where
--  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransIO where
  liftIO x = Transient $ liftIO x >>= return . Just --     let x= liftIO io in x `seq` lift x


-- * Threads

waitQSemB sem= atomicModifyIORef sem $ \n -> if n > 0 then(n-1,True) else (n,False)
signalQSemB sem= atomicModifyIORef sem  $ \n ->  (n + 1,())

-- | set the maximun number of threads for a procedure. It is useful to limit the
-- parallelization of transient code that uses `parallel` `spawn` and `waitEvents`
threads :: Int -> TransIO a -> TransIO a
threads n proc= Transient $ do
   msem <- gets maxThread
   sem <- liftIO $ newIORef n
   modify $ \s -> s{maxThread= Just sem}
   r <- runTrans proc
   modify $ \s -> s{maxThread = msem} -- restore it
   return r

-- | delete all the previous childs generated by the expressions and continue execution
-- of the current thread.
oneThread :: TransientIO a -> TransientIO a
oneThread comp=  do
   chs <- liftIO $ newTVarIO []
   r <-  comp
   modify $ \ s -> s{children= chs}
   killChilds
   return r



-- | add n threads to the limit of threads. If there is no limit, it set it
addThreads' :: Int -> TransIO ()
addThreads' n= Transient $ do
   msem <- gets maxThread
   case msem of
    Just sem -> liftIO $ modifyIORef sem $ \n' -> n + n'
    Nothing  -> do
        sem <- liftIO (newIORef n)
        modify $ \ s -> s{maxThread= Just sem}
   return $ Just ()

-- | assure that at least there are n threads available
addThreads n= Transient $ do
   msem <- gets maxThread
   case msem of
     Nothing -> return ()
     Just sem ->  liftIO $ modifyIORef sem $ \n' -> if n' > n then n' else  n
   return $ Just ()
--getNonUsedThreads :: TransIO (Maybe Int)
--getNonUsedThreads= Transient $ do
--   msem <- gets maxThread
--   case msem of
--    Just sem -> liftIO $ Just <$> readIORef sem
--    Nothing -> return Nothing


-- | The threads generated in the process passed as parameter will not be killed.
freeThreads :: TransIO a -> TransIO a
freeThreads proc= Transient $ do
     st <- get
     put st{freeTh= True}
     r <- runTrans proc
     modify $ \s -> s{freeTh= freeTh st}
     return r

-- | The threads will be killed when the parent thread dies. That is the default.
-- This can be invoked to revert the effect of `freeThreads`
hookedThreads :: TransIO a -> TransIO a
hookedThreads proc= Transient $ do
     st <- get
     put st{freeTh= False}
     r <- runTrans proc
     modify $ \st -> st{freeTh= freeTh st}
     return r

-- | kill all the child processes
killChilds :: TransientIO()
killChilds= Transient $  do
   cont <- get
   liftIO $  killChildren cont
   return $ Just ()

-- * extensible state: session data management

-- | Get the state data for the desired type if there is any.
getData ::  (MonadState EventF m,Typeable a) =>  m (Maybe a)
getData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list  of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined


-- | getData specialized for the Transient monad. if Nothing, the
-- monadic computation does not continue.
--
-- If there is no such data, `getSData`  silently stop the computation.
-- That may or may not be the desired behaviour.
-- To make sure that this does not get unnoticed, use this construction:
--
-- >  getSData <|> error "no data"
--
getSData ::  Typeable a => TransIO  a
getSData= Transient getData



-- | set session data for this type. retrieved with getData or getSData
-- Note that this is data in a state monad, that means that the update only affect downstream
-- in the monad execution. it is not a global state neither a per user or per thread state
-- it is a monadic state like the one of a state monad.
setData ::  (MonadState EventF m, Typeable a) => a -> m ()
setData  x=
  let t= typeOf x in  modify $ \st -> st{mfData= M.insert  t (unsafeCoerce x) (mfData st)}

-- | a shorter name for setData
setSData ::  (MonadState EventF m, Typeable a) => a -> m ()
setSData = setData

delSessionData x=
  modify $ \st -> st{mfData= M.delete (typeOf x ) (mfData st)}

delSData :: ( MonadState EventF m,Typeable a) => a -> m ()
delSData= delSessionData

--withSData ::  ( MonadState EventF m,Typeable a) => (Maybe a -> a) -> m ()
--withSData f= modify $ \st -> st{mfData=
--    let dat = mfData st
--        mx= M.lookup typeofx dat
--        mx'= case mx of Nothing -> Nothing; Just x -> unsafeCoerce x
--        fx=  f mx'
--        typeofx= typeOf $ typeoff f
--    in  M.insert typeofx  (unsafeCoerce fx) dat}
--    where
--    typeoff :: (Maybe a -> a) -> a
--    typeoff = undefined
----

-- | generator of identifiers that are unique withing the current monadic sequence
-- They are not unique in the whole program.
genId :: MonadState EventF m =>  m Int
genId= do
      st <- get
      let n= mfSequence st
      put st{mfSequence= n+1}
      return n

getPrevId :: MonadState EventF m =>  m Int
getPrevId= do
      n <- gets mfSequence
      return n

instance Read SomeException where
   readsPrec n str=
      let [(s , r)]= read str in [(SomeException $ ErrorCall s,r)]

-- | async calls

data StreamData a=  SMore a | SLast a | SDone | SError SomeException deriving (Typeable, Show,Read)


-- | variant of `parallel` that repeatedly executes the IO computation and kill the previously created childs
--
-- It is useful in single threaded problems where each event discard the computations spawned by
-- previous events
waitEvents ::   IO b -> TransIO b
waitEvents io= do
   SMore r <- parallel (SMore <$> io)
   return r

-- Multithreaded version of `waitEvents` that do not kill the computations spawned by previous events
waitEvents' ::   IO b -> TransIO b
waitEvents' io= do
   SMore r <- parallel (SMore <$> io)
   return r

-- | variant of `parallel` that execute the IO computation once, and kill the previous child threads
async  ::  IO b -> TransIO b
async io= do
   SLast r <- parallel  (SLast <$>io)
   return r

-- | variant that spawn free threads. Since there is no thread control, this is faster
spawn ::  IO b -> TransIO b
spawn io= freeThreads $ do
   SMore r <- parallel (SMore <$>io)
   return r





-- |  return empty to the current thread, in new thread, execute the IO action,
-- this IO action modify an internal buffer. then, executes the closure where `parallel` is located
-- In this new execution, since the buffer is filled, `parallel` return the content of this buffer.
-- Then it launch the continuation after it with this new value returned by the closure.
--
-- If the maximum number of threads, set with `threads` has been reached  `parallel` perform
-- the work sequentially, in the current thread.
-- So `parallel` means that 'it can be parallelized if there are thread available'
--
-- if there is a limitation of threads, when a thread finish, the counter of threads available
-- is increased so another `parallel` can make use of it.
--
-- The behaviour of `parallel` depend on `StreamData`; If `SMore`, `parallel` will excute again the
-- IO action. with `SLast`, `SDone` and `SError`, `parallel` will not repeat the IO action anymore.
parallel  ::    IO (StreamData b) -> TransIO (StreamData b)
parallel  ioaction= Transient $   do

    cont <- get                    -- !> "PARALLEL"
    case event cont of
     j@(Just _) -> do
        put cont{event=Nothing}
        return $ unsafeCoerce j
     Nothing -> do
        liftIO $ loop cont ioaction
        setData WasParallel
        return Nothing





-- executes the IO action and then the continuation included in the first parameter
loop :: EventF -> IO (StreamData t) -> IO ()
loop (cont'@(EventF eff e x fs a b c d _ childs g))  rec  =  do
  chs <- liftIO $ newTVarIO []
  let cont = EventF eff e x fs a b c d (Just cont') chs g
      iocont dat= do
          runStateT (runCont cont) cont{event= Just $ unsafeCoerce dat}
          return ()

      -- execute the IO computation and then the closure-continuation
      loop'= forkMaybe False cont $ do
         mdat <- rec
         case mdat of
             se@(SError _) ->  iocont se
             SDone ->          iocont SDone
             last@(SLast _) -> iocont last

             more@(SMore _) -> do
                  forkMaybe False cont $ iocont more
                  loop'

  loop'
  return ()
  where
  forkMaybe True cont proc = forkMaybe' True cont proc
  forkMaybe False cont proc = do
     dofork <- case maxThread cont of
                  Nothing -> return True
                  Just sem -> do
                    dofork <- waitQSemB sem
                    if dofork then  return True else return False
     forkMaybe' dofork cont proc

  forkMaybe' dofork cont proc=
         if dofork
            then  do
                 forkFinally1 (do
                     th <- myThreadId
                     hangThread cont' cont{threadId=th}  -- !!>  "thread created: "++ show th
                     proc)
                     $ \me -> do
                         case me of -- !> "THREAD END" of
                          Left  e -> do
                             when (fromException e /= Just ThreadKilled)$ liftIO $ print e
                             killChildren  cont  -- !!> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)

                          Right _ ->  when(not $ freeTh cont') $ do -- if was not a free thread
                             --  if parent is alive
                             --  then remove himself from the parent list (with free)
                             --  and pass his active children to his parent

                             th <- myThreadId
                             mparent <- free th cont
                             return ()

--                             case mparent of
--                              Nothing  ->  return()
--                              Just parent -> atomically $ do
--                                     chs' <- readTVar $ children cont
--                                     chs  <- (readTVar $ children parent)
--                                     writeTVar (children parent)$ chs ++ chs'
--                                     return ()

                         case maxThread cont of
                           Just sem -> signalQSemB sem
                           Nothing -> return ()
                 return ()




            else proc  -- !> "NO THREAD"

forkFinally1 :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally1 action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

free th env= do
  if isNothing $ parent env
   then  return Nothing  -- !!>  show th ++ " orphan"
   else do
    let msibling= fmap children $ parent env

    case msibling of
     Nothing -> return Nothing
     Just sibling  -> do
       found <- atomically $ do
                sbs <- readTVar sibling
                let (sbs', found) = drop [] th  sbs   -- !!> "search "++show th ++ " in " ++ show (map threadId sbs)
                when found $ writeTVar sibling sbs'    -- !!> "found"
                return found
       if (not found && isJust (parent env))
         then free th $ fromJust $ parent env         -- !!> "toparent"
         else return $ Just env

   where
   drop processed th []= (processed,False)
   drop processed th (ev:evts)| th ==  threadId ev= (processed ++ evts, True)
                    | otherwise= drop (ev:processed) th evts

hangThread parent child = when(not $ freeTh parent) $ do
   let headpths= children parent
   atomically $ do
       ths <- readTVar headpths
       writeTVar headpths $  child:ths   -- !!>  "thread added: "++ show (threadId child)

-- | kill  all the child threads associated with the continuation context
killChildren cont  = do

     forkIO $ do
        let childs= children cont
        ths <- atomically $ do
           ths <- readTVar childs
           writeTVar childs []
           return ths
--        mapM_ killChildren ths       -- recursive not needed, event handlers do it

        mapM_ (killThread . threadId) ths   -- !!> ("KILLEVENT " ++ show (map threadId ths) ++
--                                                        if length ths <20 then ""
--                                                          else error "long list of threads" )
     return ()


type EventSetter eventdata response= (eventdata ->  IO response) -> IO ()
type ToReturn  response=  IO response

-- | deinvert an event handler. The first parameter is the event handler setter to be
-- deinverted. Usually it is the primitive provided by a framework to set an event handler
--
-- the second is the value to return to the event handler
-- it configures the event handler by calling the first parameter, that set the event
-- handler, with the current continuation
react
  :: Typeable eventdata
  => EventSetter eventdata response
  -> ToReturn  response
  -> TransIO eventdata
react setHandler iob= Transient $ do
        cont    <- get
        mEvData <- getData
        case mEvData of
          Nothing -> do
            liftIO $ setHandler $ \dat ->do
              runStateT (setData dat >> runCont cont) cont
              iob
            setSData WasParallel
            return Nothing
          Just dat -> do
             delSessionData dat
             return (Just  dat)





-- * non-blocking keyboard input

getLineRef= unsafePerformIO $ newTVarIO Nothing


roption= unsafePerformIO $ newMVar []

-- | install a event receiver that wait for a string and trigger the continuation when this string arrives.
option :: (Typeable b, Show b, Read b, Eq b) =>
     b -> String -> TransIO b
option ret message= do
    let sret= show ret

    liftIO $ putStrLn $ "Enter  "++sret++"\tto: " ++ message
    liftIO $ modifyMVar_ roption $ \msgs-> return $ sret:msgs
    waitEvents  $ getLine' (==ret)
    liftIO $ putStrLn $ show ret ++ " chosen"
    return ret


-- | validates an input entered in the keyboard in non blocking mode. non blocking means that
-- the user can enter also anything else to activate other option
-- unlike `option`, wich watch continuously, input only wait for one valid response
input :: (Typeable a, Read a) => (a -> Bool) -> TransIO a
input cond= Transient . liftIO . atomically $ do
       mr <- readTVar getLineRef
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r  of
            (s,_):_ -> if cond s  --  !> show (cond s)
                     then do
                       writeTVar  getLineRef Nothing -- !>"match"
                       return $ Just s

                     else return Nothing
            _ -> return Nothing

-- | non blocking `getLine` with a validator
getLine' cond=    do
     atomically $ do
       mr <- readTVar getLineRef
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r of --  !> ("received " ++  show r ++ show (unsafePerformIO myThreadId)) of
            (s,_):_ -> if cond s -- !> show (cond s)
                     then do
                       writeTVar  getLineRef Nothing -- !>"match"
                       return s

                     else retry
            _ -> retry

reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

inputLoop=  do
    putStrLn "Press end to exit"
    inputLoop'  -- !> "started inputLoop"
    where

    inputLoop'= do
           r<- getLine
           processLine r
           inputLoop'

processLine r= do
--   when (r=="end") $ putMVar rexit ()
   let rs = breakSlash [] r
   mapM_ (\ r -> if (r=="end") then putMVar rexit () else do
                    threadDelay 1000
                    atomically . writeTVar  getLineRef $ Just r) rs

    where
    breakSlash :: [String] -> String -> [String]
    breakSlash s ""= s
    breakSlash res s=
      let (r,rest) = span(/= '/') s
      in breakSlash (res++[r]) $ tail1 rest
      where
      tail1 []=[]
      tail1 x= tail x


rexit= unsafePerformIO newEmptyMVar


stay=  takeMVar rexit


-- | keep the main thread running, initiate the asynchronous keyboard input and execute
-- the transient computation. It also read a slash separated list of string that are interpreted by
-- `option` and `input` as if they were entered by the keyboard
keep :: TransIO a -> IO a
keep mx = do
   forkIO inputLoop
   forkIO $ do
     runTransient $ do
--       async inputLoop <|> return ()
       mx
     return ()
   threadDelay 100000
   args <- getArgs
   let path = filter (\arg -> arg !! 0 == '/') args
   when (not (null path)) $ do
        putStr "Executing: " >> print (head path)
        processLine $  head path
   stay

-- | same than `keep`but do not initiate the asynchronous keyboard input.
-- Useful for debugging
keep' :: TransIO a -> IO a
keep' mx  = do

   forkIO $ runTransient mx  >> return ()

   stay

-- | force the finalization of the main thread and thus, all the application
exit :: TransIO a
exit= do
  liftIO $ putMVar rexit   True
  stop

-- | alternative operator for maybe values. Used  in infix mode
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'
