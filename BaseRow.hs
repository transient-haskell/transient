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

-- show
module BaseRow  where 
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

import           Data.Maybe
import           GHC.Conc
import           Data.List

(!>) =   flip trace
infixr 0 !>

data Transient m x= Transient  {runTrans :: m (Maybe x)}
type SData= ()

type EventId= Int



data EventF  = forall a b . EventF{xcomp      :: TransientIO a
                                  ,fcomp      :: a -> TransientIO b
                                  ,mfData     :: M.Map TypeRep SData
                                  ,mfSequence :: Int
                                  ,replay     :: Bool
                                  ,newRow     :: Bool
                                  ,children   :: P [ThreadId]
                                  ,rowChildren ::P [ThreadId]
                                  }

type P= MVar 

(=:) :: P a  -> a -> IO()
(=:) n v= modifyMVar_ n $ const $ return v

type Buffer= P (Maybe ())



instance Show x => Show (MVar x) where
  show  x = show (unsafePerformIO $ readMVar x)

eventf0=  EventF  empty (const  empty) M.empty 0
          False False (unsafePerformIO $ newMVar [])  (unsafePerformIO $ newMVar [])


topNode= (-1 :: Int,unsafePerformIO $ myThreadId,Nothing)

--{-#NOINLINE rootRef#-}
--rootRef :: P Node
--rootRef=  unsafePerformIO $ newMVar $ Node topNode $ unsafePerformIO $ newMVar []

instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

--runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
--runTrans (Transient mx) = mx

runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= runStateT (runTrans t) eventf0


setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF   _ fs d n  r nr  ch rc)  <- get
   put $ EventF   x ( f >=> unsafeCoerce fs) d n  r nr  ch rc
   return st


resetEventCont (EventF x fs _ _   _ _ _ _)=do
   EventF   _ _ d  n   r nr  ch rc <- get
   put $ EventF  x fs d n   r True   ch rc 


getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO (Maybe b)
runCont (EventF  x f _ _  _ _  _ _)= runTrans $ x' >>=  unsafeCoerce f
    where 
    x'= do
       modify $ \s -> s{replay=True}
       x
       modify $ \s -> s{replay=False,newRow=True}
       
  


runClosure :: EventF -> StateIO (Maybe a)
runClosure (EventF x _ _ _ _ _ _ _) =  unsafeCoerce $ runTrans x

runContinuation ::  EventF -> a -> StateIO (Maybe b)
runContinuation (EventF _ fs _ _ _ _  _ _) x= runTrans $  (unsafeCoerce fs) x

instance   Functor TransientIO where
  fmap f x=   Transient $ fmap (fmap f) $ runTrans x --


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
        resetEventCont cont
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
      case replay st of
        True -> do
          let n= mfSequence st
          put $ st{mfSequence= n+1}
          return n
        False -> liftIO $
          modifyMVar refSequence $ \n -> return (n+1,n)

refSequence :: MVar Int
refSequence= unsafePerformIO $ newMVar 0



data Loop= Once | Loop | Multithread deriving Eq

waitEvents ::  IO b -> TransientIO b
waitEvents= parallel Loop


async  :: IO b -> TransientIO b
async = parallel Once

spawn= parallel Multithread


parallel  ::  Loop ->  IO b -> TransientIO b
parallel looptype ioaction= Transient $ do
        let buffer = unsafePerformIO $  newMVar Nothing 
        mEvData <- liftIO $ readMVar buffer
        case mEvData of
          Nothing ->do
              return () !> "NOTHING " ++ (show $ unsafePerformIO myThreadId)
              cont    <- getCont

              let    newr = newRow cont
              when (not newr) $ liftIO $ modifyMVar_ buffer . const . return $ Just Nothing

              npchildren <-  do
                    if newr then do
                          np <- liftIO $ newMVar []
                          put cont{rowChildren= np,newRow=False}
                          return np
                    else return $ rowChildren cont 

              return () !> "NEWROW="++ show newr

              let cont'=  cont{children= npchildren,newRow=False}
              th <- liftIO $ forkIO $ loop npchildren looptype  ioaction $ \dat -> do
                      modifyMVar_ buffer . const . return $ Just (Just dat)
                      (flip runStateT) cont' $ do
                                 pchildren <- gets children
                                 ts <- liftIO $ readMVar npchildren
                                 liftIO $ mapM_ killThread ts !>  ("KILL" ++ show ts)
                                 runCont cont'
                      return ()
                      
              liftIO $ modifyMVar_ (children cont) $ \ths -> return $ th:ths
              return Nothing
              
          Just Nothing  -> return Nothing   !> "NODATA " ++ (show $ unsafePerformIO myThreadId)
          
          Just (Just dat) -> do
              return $ Just dat !> "JUST " ++ (show $ unsafePerformIO myThreadId)

    where
    killChildren pch= do
        ths <- readMVar pch 
        mapM_ killThread ths !> "KILLEVENT "++ show ths

    loop pch t rec f = handle (\(e::SomeException) -> killChildren pch) $ loop' t rec f  !> "FORK"
    loop' Once rec f = rec >>= f
    
    loop' Loop rec f = do
            r <-  rec  
            f r
            loop' Loop rec f  
            
    loop' Multithread rec f = do
            r <- rec
            forkIO $ f r
            loop' Multithread rec f


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

--hash f= liftIO $ do
--          st <- makeStableName $! f `seq` f
--          return $hashStableName st

--uhash= unsafePerformIO .hash

getLineRef= unsafePerformIO $ newTVarIO Nothing


option1 x  message= waitEvents  $ do
     liftIO $ putStrLn $ message++"("++show x++")"
     atomically $ do
       mr <- readTVar getLineRef  
       th <- unsafeIOToSTM myThreadId  
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r !> ("received " ++  show r ++  show th) of
            (s,_):_ -> if  s == x  !> ("waiting" ++ show x)
                     then do
                       writeTVar  getLineRef Nothing !>"match"
                       return s

                     else retry
            _ -> retry
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

option ret message= do
    liftIO $ putStrLn $"Enter "++show ret++"\tto: " ++ message
    waitEvents  $ getLine' (==ret)
    liftIO $do putStrLn $ show ret ++ " chosen"
    return ret

getLine' cond=   inputLoop `seq` do
     atomically $ do
       mr <- readTVar getLineRef
       th <- unsafeIOToSTM myThreadId
       case mr of
         Nothing -> retry
         Just r ->
            case reads1 r !> ("received " ++  show r ++ show th) of
            (s,_):_ -> if cond s  !> show (cond s)
                     then do
                       writeTVar  getLineRef Nothing !>"match"
                       return s

                     else retry
            _ -> retry
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

inputLoop=  do
    print "Press end to exit"
    inputLoop'
    where
        inputLoop'= do
           r<- getLine                      !> "started inputLoop"
           if r=="end" then putMVar rexit () else do
              atomically . writeTVar  getLineRef $ Just r
              inputLoop'


rexit= unsafePerformIO newEmptyMVar

stay=  takeMVar rexit

onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'
