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
-- |
--
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

(!>) =   const . id -- flip trace
infixr 0 !>

data Transient m x= Transient  {runTrans :: m (Maybe x)}
type SData= ()

type EventId= Int



data EventF  = forall a b . EventF{xcomp       :: TransientIO a
                                  ,fcomp       :: [b -> TransientIO b]
                                  ,mfData      :: M.Map TypeRep SData
                                  ,mfSequence  :: Int
                                  ,replay      :: Bool
                                  ,freeTh      :: Bool
                                  ,parent      :: EventF
                                  ,children    :: P[ThreadId]
                                  ,maxThread     :: Maybe (P Int)
                                  }
                                  deriving Typeable

type P= IORef
newp= newIORef


--(=:) :: P a  -> (a -> a) -> IO()
(=:) n f= liftIO $ atomicModifyIORef' n $  \v ->  ((f v),())

addr x= show $ unsafePerformIO $ do
       st <- makeStableName $! x
       return $ hashStableName st

waitQSemB sem= atomicModifyIORef' sem $ \n -> if n > 0 then(n-1,True) else (n,False)
signalQSemB sem= atomicModifyIORef' sem  $ \n ->  (n + 1,())

-- | set the maximun number of threads for a procedure. It is useful for the
threads :: Int -> TransientIO a -> TransientIO a
threads n proc= Transient $do
   msem <- gets maxThread
   sem <- liftIO $ newIORef n
   modify $ \s -> s{maxThread= Just sem}
   r <- runTrans proc
   modify $ \s -> s{maxThread = msem} -- restore it
   return r


instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

--runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
--runTrans (Transient mx) = mx

runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= do
  childs  <- newp []
  let eventf0=  EventF  empty [] M.empty 0
          False False  eventf0  childs Nothing

  runStateT (runTrans t) eventf0


setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO ()
setEventCont x f  = do
   st@(EventF   _ fs d n  r applic  ch rc bs)  <- get
--   if applic
--    then return () else
   put $ EventF x ( unsafeCoerce f : fs) d n  r applic  ch rc bs



resetEventCont :: Maybe a -> StateIO ()
resetEventCont mx =do
   st@(EventF _ fs d n  r nr  ch rc bs)  <- get
--   if nr
--    then return ()
--    else do
   let f= \mx ->  case mx of
                       Nothing -> empty
                       Just x  -> (unsafeCoerce $ head fs)  x
   put $ EventF  (f mx) ( tailsafe fs)d n  r nr  ch rc bs
   where
   tailsafe []=[]
   tailsafe (x:xs)= xs



getCont ::(MonadState EventF  m) => m EventF
getCont = get


runCont :: EventF -> StateIO ()
runCont (EventF  x fs _ _  _ _  _ _ _)= runTrans ((unsafeCoerce x') >>= compose ( fs)) >> return ()
    where
    x'=  do
           modify $ \s -> s{replay=True}
           r<- x
           modify $ \s -> s{replay=False}
           return r

{-
runCont cont= do
     mr <- runClosure cont
     case mr of
         Nothing -> return Nothing
         Just r -> runContinuation cont r
-}

compose []= const empty
compose (f: fs)= \x -> f x >>= compose fs



runClosure :: EventF -> StateIO (Maybe a)
runClosure (EventF x _ _ _ _ _ _ _ _) =  unsafeCoerce $ runTrans x

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
         liftIO $ writeIORef rf  k -- :: StateIO ()

         put $ EventF g (unsafeCoerce appg :  fs)
                                          a b c d peers  children g1
         x <- runTrans g
         liftIO $ writeIORef rg  x

         return $ k <*> x


instance  Alternative TransientIO where
  empty= Transient $ return  Nothing
  Transient f <|> Transient g= Transient $ do
         k <-   f
         x <-   g
         return $ k <|> x

-- | delete all the previous childs generated by the expressions and continue execution
-- of the current thread.
oneThread :: TransientIO a -> TransientIO a
oneThread comp=  do
   chs <- liftIO $ newp []
   r <- comp
   modify $ \ s -> s{children= chs}
   killchilds
   return r

-- | internal. use `oneThread`
killchilds :: TransientIO()
killchilds= Transient $  do
   cont <- get
   liftIO $  killChildren cont
   return $ Just ()

-- | The threads generated in the process passed as parameter will not be killed.
freeThreads :: TransientIO a -> TransientIO a
freeThreads proc= Transient $ do
     st <- get
     put st{freeTh= True}
     r <- runTrans proc
     modify $ \st -> st{freeTh= freeTh st}
     return r

-- | The threads will be killed when the parent thread dies
hookedThreads proc= Transient $ do
     st <- get
     put st{freeTh= False}
     r <- runTrans proc
     modify $ \st -> st{freeTh= freeTh st}
     return r

instance MonadPlus TransientIO where
    mzero= stop
    mplus (Transient x) (Transient y)=  Transient $ do
         mx <- x
         case mx of
             Nothing -> y
             justx -> return justx

-- | a sinonym of empty that can be used in a monadic expression. it stop the
-- computation
stop :: TransientIO a
stop= Control.Applicative.empty

instance Monoid a => Monoid (TransientIO a) where
  mappend x y = mappend <$> x <*> y
  mempty= return mempty

instance Monad TransientIO where
      return x = Transient $ return $ Just x
      x >>= f  = Transient $ do
        cont <- setEventCont x  f

        mk <- runTrans x
        resetEventCont mk
        case mk of
           Just k  -> do
               runTrans $ f k

           Nothing -> return Nothing



instance MonadTrans (Transient ) where
  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransientIO where
  liftIO = lift . liftIO --     let x= liftIO io in x `seq` lift x



-- | Get the session data of the desired type if there is any.
getSessionData ::  (MonadState EventF m,Typeable a) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | getSessionData specialized for the View monad. if Nothing, the
-- monadic computation does not continue. getSData is a widget that does
-- not validate when there is no data of that type in the session.
getSData :: MonadState EventF m => Typeable a =>Transient m  a
getSData= Transient getSessionData


-- | setSessionData ::  (StateType m ~ MFlowState, Typeable a) => a -> m ()
setSessionData  x=
  modify $ \st -> st{mfData= M.insert  (typeOf x ) (unsafeCoerce x) (mfData st)}

-- | a shorter name for setSessionData
setSData ::  ( MonadState EventF m,Typeable a) => a -> m ()
setSData= setSessionData

delSessionData x=
  modify $ \st -> st{mfData= M.delete (typeOf x ) (mfData st)}

delSData :: ( MonadState EventF m,Typeable a) => a -> m ()
delSData= delSessionData

withSData ::  ( MonadState EventF m,Typeable a) => (Maybe a -> a) -> m ()
withSData f= modify $ \st -> st{mfData=
    let dat = mfData st
        mx= M.lookup typeofx dat
        mx'= case mx of Nothing -> Nothing; Just x -> unsafeCoerce x
        fx=  f mx'
        typeofx= typeOf $ typeoff f
    in  M.insert typeofx  (unsafeCoerce fx) dat}
    where
    typeoff :: (Maybe a -> a) -> a
    typeoff = undefined
----

genNewId :: MonadIO m => MonadState EventF m =>  m Int
genNewId=  do
          st <- get

          let n= mfSequence st
          put $ st{mfSequence= n+1}
          return n

refSequence :: IORef Int
refSequence= unsafePerformIO $ newp 0



data Loop= Once | Loop | Multithread deriving Eq

waitEvents ::   IO b -> TransientIO b
waitEvents io= do
   r <- parallel (Right <$> io)
   killchilds
   return r


async  ::  IO b -> TransientIO b
async io= do
   r <- parallel  (Left <$>io)
   killchilds
   return r

spawn ::  IO b -> TransientIO b
spawn io= freeThreads $ do
   r <- parallel (Right <$>io)
   return r

data EventValue= EventValue SData deriving Typeable

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
  chs <- newp []
  let cont = EventF x fs a b c d cont' chs g
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
              forkMaybe  $ iocont dat
              loop'

      forkMaybe  proc = do
        case maxThread cont of
         Nothing ->  do
            th <- forkFinally proc $ \me -> do
                   case me !> "THREAD ENDED" of
                    Left  e -> do
--                       when (fromException e /= Just ThreadKilled)$ liftIO $
                       print e  -- !> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)
                       killChildren  cont

                    Right _ -> return ()
            addThread cont' th !>  "thread created: "++ show th
         Just sem -> do
          dofork <- waitQSemB sem
          if dofork
            then  do
                 th <- forkFinally proc $ \me -> do
                         signalQSemB sem
                         case me !> "THREAD ENDED" of
                          Left  e -> do
--                           when (fromException e /= Just ThreadKilled)$ liftIO $
                           print e   -- !> "KILL RECEIVED" ++ (show $ unsafePerformIO myThreadId)
                           killChildren  cont

                          Right _ -> return ()
                 addThread cont' th  !>  "thread created: "++ show th

            else proc  -- !> "NO THREAD"


  forkMaybe  loop'

addThread cont' th= when(not$ freeTh cont') $ do
--   liftIO $ print $ "ADDTHREAD  to" ++ addr (children cont')
   let headpths= children cont'
   (headpths) =:  \ths -> th:ths

killChildren  cont=do
--     th <- myThreadId -- !> "KILLCHILDREN"
     forkIO $ do
        let childs= children cont --  !> "killChildren list= "++ addr (children cont)
        ths <- liftIO $ atomicModifyIORef' childs $ \ths ->([],ths)
        mapM_ killThread  ths  !> "KILLEVENT " ++ show ths
     return ()


type EventSetter eventdata response= (eventdata ->  IO response) -> IO ()
type ToReturn  response=  IO response
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



getLineRef= unsafePerformIO $ newTVarIO Nothing


option1 x  message= do

 waitEvents  $ do
     liftIO $ putStrLn $ message++"("++show x++")"
     th <-  myThreadId
     atomically $ do
       mr <- readTVar getLineRef

       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r of  -- !> ("received " ++  show r ++  show th) of
            (s,_):_ -> if  s == x  -- !> ("waiting" ++ show x)
                     then do
                       writeTVar  getLineRef Nothing -- !>"match"
                       return s

                     else retry
            _ -> retry
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined


roption= unsafePerformIO $ newMVar []

-- | install a event receiver that wait for a string and trigger the continuation when this string arrives.
option :: (Typeable b, Show b, Read b, Eq b) =>
     b -> [Char] -> TransientIO b
option ret message= do
    let sret= show ret

    liftIO $ putStrLn $ "Enter "++sret++"\tto: " ++ message
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
              atomically . writeTVar  getLineRef $ Just r
              inputLoop'


rexit= unsafePerformIO newEmptyMVar

stay=  takeMVar rexit

keep mx = do
   forkIO $ inputLoop
   forkIO $ runTransient mx  >> return ()
   stay

exit :: TransientIO a
exit= do
  liftIO $ putStrLn "Tempus fugit: exit"
  liftIO $ putMVar rexit   True
  return undefined

onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'
