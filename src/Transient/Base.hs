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


{-# INLINE (!>) #-}
(!>) = const . id -- flip trace
infixr 0 !>

data TransIO  x = Transient  {runTrans :: StateT EventF IO (Maybe x)}
type SData= ()

type EventId= Int

type TransientIO= TransIO

data EventF  = forall a b . EventF{xcomp       :: TransientIO a
                                  ,fcomp       :: [b -> TransientIO b]
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


--(=:) :: P a  -> (a -> a) -> IO()
(=:) n f= liftIO $ atomicModifyIORef n $  \v ->  ((f v),())

addr x= show $ unsafePerformIO $ do
       st <- makeStableName $! x
       return $ hashStableName st




instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

--type TransientIO= Transient StateIO

--runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
--runTrans (Transient mx) = mx


runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= do

  th <- myThreadId
  let eventf0=  EventF  empty [] M.empty 0
          th False  Nothing  (unsafePerformIO $ newTVarIO []) Nothing


  runStateT (runTrans t) eventf0{threadId=th} !> "MAIN="++show th




-- | get the continuation context: closure, continuation, state, child threads etc
getCont ::(MonadState EventF  m) => m EventF
getCont = get

-- | run the continuation context
runCont :: EventF -> StateIO ()
runCont (EventF  x fs _ _  _ _  _ _ _)= runTrans ((unsafeCoerce x') >>= compose ( fs)) >> return ()
    where
    x'=  do
--           modify $ \s -> s{replay=True}
           r<- x
--           modify $ \s -> s{replay=False}
           return r

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
runClosure (EventF x _ _ _ _ _ _ _ _) =  unsafeCoerce $ runTrans x

-- | run the continuation (the 'f' in 'x >> f') of the current bind operation
runContinuation ::  EventF -> a -> StateIO (Maybe b)
runContinuation (EventF _ fs _ _ _ _  _ _ _) x= runTrans $  (unsafeCoerce $ compose $  fs) x

instance   Functor TransientIO where
  fmap f mx=   -- Transient $ fmap (fmap f) $ runTrans mx
    do
     x <- mx
     return $ f x

instance Applicative TransientIO where
  pure a  = Transient . return $ Just a


  f <*> g = Transient $ do

         rf <- liftIO $ newIORef Nothing
         rg <- liftIO $ newIORef Nothing   -- !> "NEWIOREF"

         cont@(EventF _ fs a b c d peers children g1) <- get   -- !> "APLICATIVE DOIT"

         let
             appg x = Transient $  do
                   liftIO $ writeIORef rg $ Just x :: StateIO ()
                   k <- liftIO $ readIORef rf
                   return $ k <*> Just x  -- !> "RETURNED: " ++ show(isJust k)++ show(isJust x)


             appf k = Transient $  do
                   liftIO $ writeIORef rf  $ Just k :: StateIO ()
                   x<- liftIO $ readIORef rg
                   return $ Just k <*> x  --  !> "RETURNED: " ++ show(isJust k)++ show(isJust x)



         put $ EventF f (unsafeCoerce appf:  fs)
                                          a b c d peers children g1
         k <- runTrans f
         was <- getSessionData `onNothing` return NoRemote
         if was== WasRemote
           then  return Nothing
           else do
             liftIO $ writeIORef rf  k -- :: StateIO ()

             mfdata <- gets mfData
             put $ EventF g (unsafeCoerce appg :  fs) mfdata b c d peers  children g1


             x <- runTrans g              !> "RUN g"
             liftIO $ writeIORef rg  x
             return $ k <*> x

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
data LogElem=  WaitRemote | Exec | Step IDynamic deriving (Read,Show)
data Log= Log Recover  CurrentPointer LogEntries deriving Typeable


instance  Alternative TransientIO where
  empty = Transient $ return  Nothing
  (<|>) = mplus

--  Transient f <|> Transient g= Transient $ do
--         k <-   f
--         x <-   g
--         return $ k <|> x

data RemoteStatus=  WasRemote | NoRemote deriving (Typeable, Eq)

instance MonadPlus TransientIO where
    mzero= empty
    mplus  x y=  Transient $ do
         mx <- runTrans x    -- !> "RUNTRANS11111"
         was <- getSessionData `onNothing` return NoRemote
         if was== WasRemote
           then return Nothing
           else case mx of
             Nothing -> runTrans y   --  !> "RUNTRANS22222"
             justx -> return justx

-- | a sinonym of empty that can be used in a monadic expression. it stop the
-- computation
stop :: TransientIO a
stop= Control.Applicative.empty

instance Monoid a => Monoid (TransientIO a) where
  mappend x y = mappend <$> x <*> y
  mempty= return mempty

-- | set the current closure and continuation for the current statement
setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO ()
setEventCont x f  = do
   st@(EventF   _ fs d n  r applic  ch rc bs)  <- get
   put $ EventF x ( unsafeCoerce f : fs) d n  r applic  ch rc bs


-- | reset the closure and continuation. remove inner binds than the prevous computations may have stacked
-- in the list of continuations.
resetEventCont :: Maybe a -> StateIO ()
resetEventCont mx =do
   st@(EventF _ fs d n  r nr  ch rc bs)  <- get

   let f= \mx ->  case mx of
                       Nothing -> empty
                       Just x  -> (unsafeCoerce $ head fs)  x
   put $ EventF  (f mx) ( tailsafe fs)d n  r nr  ch rc bs
   where
   tailsafe []=[]
   tailsafe (x:xs)= xs


instance Monad TransientIO where
      return x = Transient $ return $ Just x
      x >>= f  = Transient $ do
        setEventCont x  f

        mk <- runTrans x
        resetEventCont mk
        case mk of
           Just k  -> do
               runTrans $ f k

           Nothing -> return Nothing




--instance MonadTrans (Transient ) where
--  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransientIO where
  liftIO x = Transient $ liftIO x >>= return . Just --     let x= liftIO io in x `seq` lift x




-- * Threads

waitQSemB sem= atomicModifyIORef sem $ \n -> if n > 0 then(n-1,True) else (n,False)
signalQSemB sem= atomicModifyIORef sem  $ \n ->  (n + 1,())

-- | set the maximun number of threads for a procedure. It is useful to limit the
-- parallelization of transient code that uses `parallel` `spawn` and `waitEvents`
threads :: Int -> TransientIO a -> TransientIO a
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
   r <- comp
   modify $ \ s -> s{children= chs}
   killChilds
   return r



-- | The threads generated in the process passed as parameter will not be killed.
freeThreads :: TransientIO a -> TransientIO a
freeThreads proc= Transient $ do
     st <- get
     put st{freeTh= True}
     r <- runTrans proc
     modify $ \st -> st{freeTh= freeTh st}
     return r

-- | The threads will be killed when the parent thread dies. That is the default.
-- This can be invoked to revert the effect of `freeThreads`
hookedThreads :: TransientIO a -> TransientIO a
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

-- | Get the session data for the desired type if there is any.
getSessionData ::  (MonadState EventF m,Typeable a) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list  of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | getSessionData specialized for the Transient monad. if Nothing, the
-- monadic computation does not continue.
--
-- If there is no such data, `getSData`  silently stop the computation.
-- That may or may not be the desired behaviour.
-- To make sure that this does not get unnoticed, use this construction:
--
-- >  getSData <|> error "no data"
--
getSData ::  Typeable a => TransIO  a
getSData= Transient getSessionData


-- | set session data for this type. retrieved with getSessionData orr getSData
setSessionData ::  (MonadState EventF m, Typeable a) => a -> m ()
setSessionData  x= do
  let t= typeOf x in  modify $ \st -> st{mfData= M.insert  t (unsafeCoerce x) (mfData st)}

-- | a shorter name for setSessionData
setSData ::  ( MonadState EventF m,Typeable a) => a -> m ()
setSData= setSessionData

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

-- | generator of identifiers
genNewId :: MonadIO m => MonadState EventF m =>  m Int
genNewId=  do
          st <- get

          let n= mfSequence st
          put $ st{mfSequence= n+1}
          return n

refSequence :: IORef Int
refSequence= unsafePerformIO $ newp 0



data Loop= Once | Loop | Multithread deriving Eq

-- | variant of `parallel` that repeatedly executes the IO computation and kill the previously created childs
waitEvents ::   IO b -> TransientIO b
waitEvents io= do
   r <- parallel (Right <$> io)
   killChilds
   return r

-- | variant of + parallel` that execute the IO computation once, and kill the previous child threads
async  ::  IO b -> TransientIO b
async io= do
   r <- parallel  (Left <$>io)
   killChilds
   return r

-- | variant that spawn free threads. Since there is not thread control, this is faster
spawn ::  IO b -> TransientIO b
spawn io= freeThreads $ parallel (Right <$>io)


data EventValue= EventValue SData deriving Typeable

-- |  return empty to the current thread and launch the IO action in a new thread and attaches the continuation after it.
-- if the result of the action is `Right` the process is repeated. if not, it finish.
--
-- If the maximum number of threads, set with `threads` has been reached  `parallel` perform
-- the work sequentially, in the current thread.
--
-- when finish, increase the counter of threads available, if there is a limitation of them.

parallel  ::    IO (Either b b) -> TransientIO b
parallel  ioaction= Transient$   do
    cont <- getCont                    -- !> "PARALLEL"
    mv <- getSessionData
    case mv  of
     Just (EventValue v)  -> do
        delSessionData $ EventValue () -- !> "ISJUST "
        return  $ Just $ unsafeCoerce v
     Nothing -> do
        liftIO  $ loop cont    ioaction
        return Nothing




loop (cont'@(EventF x fs a b c d peers childs g))  rec  =  do
  chs <- liftIO $ newTVarIO []
  let cont = EventF x fs a b c d (Just cont') chs g
      iocont dat=
          runStateT ( do
             setSessionData . EventValue $ unsafeCoerce dat
             runCont cont
             ) cont
             >> return ()
      loop'= do
        mdat <- rec
        case mdat of
         Left dat -> iocont dat

         Right dat -> do
              forkMaybe cont $ iocont dat
              loop'

  forkMaybe  cont loop'
  where
      forkMaybe cont proc = do
         dofork <- case maxThread cont of
                  Nothing -> return True
                  Just sem -> do
                    dofork <- waitQSemB sem
                    if dofork then  return True else return False

         if dofork
            then  do
                 th <- forkFinally1 proc $ \me -> do
                         case me of -- !> "THREAD ENDED" of
                          Left  e -> do
                           when (fromException e /= Just ThreadKilled)$ liftIO $ print e
                           killChildren  cont  !> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)

                          Right _ -> do
                             --  if parent is alive
                             --  then remove himself from the list (with free)
                             --  and pass his active children to his parent
                             return ()
                             th <- myThreadId
                             mparent <- free th cont
                             case mparent of
                              Nothing  ->  return()
                              Just parent -> atomically $ do
                                     chs' <- readTVar $ children cont
                                     chs  <- (readTVar $ children parent)
                                     writeTVar (children parent)$ chs ++ chs'
                                     return ()

                         case maxThread cont of
                           Just sem -> signalQSemB sem
                           Nothing -> return ()


                 addThread cont' cont{threadId=th}  -- !>  "thread created: "++ show th

            else proc  -- !> "NO THREAD"

forkFinally1 :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally1 action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

free th env= do
  if isNothing $ parent env
   then  return Nothing  !>  show th ++ " orphan"
   else do
    let msibling= fmap children $ parent env

    case msibling of
     Nothing -> return Nothing
     Just sibling  -> do
       found <- atomically $ do
                sbs <- readTVar sibling
                let (sbs', found) = drop [] th  sbs -- !> "search "++show th ++ " in " ++ show (map threadId sbs)
                when found $ writeTVar sibling sbs'
                return found
       if (not found && isJust (parent env))
         then free th $ fromJust $ parent env -- !> "toparent"
         else return $ Just env

   where
   drop processed th []= (processed,False)
   drop processed th (ev:evts)| th == threadId ev= (processed ++ evts, True)
                    | otherwise= drop (ev:processed) th evts


addThread parent child = when(not $ freeTh parent) $ do
   let headpths= children parent
   atomically $ do
       ths <- readTVar headpths
       writeTVar headpths $  child:ths

-- | kill all the threads associated with the continuation context
killChildren  cont = do
     forkIO $ do
        let childs= children cont   !> "killChildren list= "++ addr (children cont)
        ths <- atomically $ do
           ths <- readTVar childs
           writeTVar childs []
           return ths
        mapM_ (killThread . threadId) ths  !> "KILLEVENT " ++ show (map threadId ths)
     return ()


type EventSetter eventdata response= (eventdata ->  IO response) -> IO ()
type ToReturn  response=  IO response

-- | deinvert an event handler. The first parameter is the event handler to be deinverted
-- the second is the value to return to the event handler
-- it configures the event handler by calling the first parameter, that set the event
-- handler, with the current continuation
react
  :: Typeable eventdata
  => EventSetter eventdata response
  -> ToReturn  response
  -> TransientIO eventdata
react setHandler iob= Transient $ do
        cont    <- getCont
        mEvData <- getSessionData
        case mEvData of
          Nothing -> do
            liftIO $ setHandler $ \dat ->do
--              let cont'= cont{mfData = M.insert (typeOf dat)(unsafeCoerce dat) (mfData cont)}
              runStateT (setSData dat >> runCont cont) cont
              iob
            return Nothing
          Just dat -> delSessionData dat >> return (Just  dat)


-- * non-blocking keyboard input

getLineRef= unsafePerformIO $ newTVarIO Nothing


roption= unsafePerformIO $ newMVar []

-- | install a event receiver that wait for a string and trigger the continuation when this string arrives.
option :: (Typeable b, Show b, Read b, Eq b) =>
     b -> [Char] -> TransientIO b
option ret message= do
    let sret= show ret

    liftIO $ putStrLn $ "Enter  "++sret++"\tto: " ++ message
    liftIO $ modifyMVar_ roption $ \msgs-> return $ sret:msgs
    waitEvents  $ getLine' (==ret)
    liftIO $ putStrLn $ show ret ++ " chosen"
    return ret


-- | validates an input entered in the keyboard in non blocking mode. non blocking means that
-- the user can enter also anything else to activate other option
-- unlike `option`, input only wait for one valid response
input :: (Typeable a, Read a) => (a -> Bool) -> TransientIO a
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
           if r=="end" then putMVar rexit () else do
              let rs = breakSlash [] r
              mapM_ (\ r -> do threadDelay 1000
                               atomically . writeTVar  getLineRef $ Just r) rs
              inputLoop'

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
-- the transient computation.
keep mx = do
   forkIO $ inputLoop
   forkIO $ runTransient mx  >> return ()
   stay

-- | force the finalization of the main thread and thus, all the application
exit :: TransientIO a
exit= do
  liftIO $ putStrLn "Tempus fugit: exit"
  liftIO $ putMVar rexit   True
  return undefined

-- | alternative operator for maybe values. Used  in infix mode
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'
