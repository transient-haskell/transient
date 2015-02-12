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
{-# LANGUAGE ExistentialQuantification,FlexibleContexts,
             FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

-- show
module Base where
-- /show

import Control.Monad.State
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic
import Debug.Trace
import Data.Monoid

--import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Conc
import Data.Maybe
import System.Mem.StableName
import Data.List

(!>) =   flip trace
infixr 0 !>

data Transient m x= Transient  (m (Maybe x))
type SData= ()

type EventId= Int

data EventF  = forall a b . EventF{xcomp :: (TransientIO a)
                                  ,fcomp :: [a -> TransientIO b]
                                  ,mfData :: M.Map TypeRep SData
                                  ,mfSequence :: Int
                                  ,eventId :: EventId
                                  ,eventHandlers ::  MVar (Maybe EventNode)
                                  ,inspected :: [EventId]}


type Inspected= Bool
type Buffer= Maybe ()
data EventNode= EventNode(EventId, ThreadId,Bool, Buffer, MVar (Maybe EventNode)) deriving Show


--instance Show EventTree where
--   show (Evntt [])= ""
--   show (Evntt ((id, th,i,buf,ior): es))= show id ++":"++ show th ++ show ior++"\n"++ show (Evntt es)

instance Show x => Show (MVar x) where
  show  x = "-> "++show (unsafePerformIO $ readMVar x)

eventf0= EventF  (empty) [const $ empty] M.empty 0 0 rootRef []


emptyNode=EventNode (-1,error "empty th in ref",False,Nothing,error "empty node ref")

rootRef :: MVar (Maybe EventNode)
rootRef=  unsafePerformIO $ newMVar $ Nothing                        

instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
runTrans (Transient mx) = mx

runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= runStateT (runTrans t) eventf0

setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF   x' fs d n id es ins)  <- get
   put $ EventF   x ( f: unsafeCoerce fs) d n id es ins
   return st

resetEventCont (EventF x fs _ _ _ _ _ )=do
   st@(EventF   _ _ d n id es ins)  <- get
   put $ EventF  x fs d n n es ins


getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF  x fs _ _ _ _ _)= do runIt x (unsafeCoerce fs); return ()
   where
      runIt x fs= runTrans $  x >>= compose fs

compose []= const empty
compose (f: fs)= \x -> f x >>= compose fs




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


-- | a sinonym of empty that can be used in a monadic expression. it stop the
-- computation
stop :: TransientIO a
stop= Control.Applicative.empty

instance Monoid a => Monoid (TransientIO a) where
  mappend x y = mappend <$> x <*> y  
  mempty= return mempty

instance Monad TransientIO where
      return x = Transient $ return $ Just x
      x >>= f = Transient $ do
        cont <- setEventCont x  f
        mk <- runTrans x
        resetEventCont cont
        case mk of
           Just k  -> runTrans $ f k

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

-- | getSessionData specialized for the View monad. if Nothing, the monadic computation
-- does not continue. getSData is a widget that does not validate when there is no data
--  of that type in the session.
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


----

genNewId ::  MonadState EventF m =>  m Int
genNewId=  do
      st <- get
      let n= mfSequence st
      put $ st{mfSequence= n+1}
      return n

--- IO events

--buffers :: IORef [(EventId,Dynamic)]
--buffers= unsafePerformIO $ newIORef []

data Loop= Once | Loop | Multithread

waitEvents ::  IO b -> TransientIO b
waitEvents= parallel Loop


async  :: IO b -> TransientIO b
async = parallel Once

parallel  ::  Loop ->  IO b -> TransientIO b
parallel hasloop receive =  Transient $ do
      id <- genNewId
      cont <- getCont
      let ref= eventHandlers cont
      th <- liftIO $myThreadId
      EventNode node@(id',_,inspectedValue, mrec,_)  <-   liftIO $ lookTree id rootRef                                   
      case id'== -1 of
        True ->do
          mcont <- liftIO $ do

               case id==id' of
                 True -> return cont{inspected= id:inspected  cont}  !> "found"
                 False ->  do
                   ref' <- newMVar Nothing !> "set reference"
                   modifyMVar_  ref $ \_-> return $ Just $ EventNode (id,unsafePerformIO myThreadId,False,Nothing,ref')
                   let cont'= cont{mfSequence= eventId cont, eventHandlers= ref'}
                   th' <- forkIO $ do
                     loop hasloop  receive $ \r -> do
--                      r <- receive

                      th <-  myThreadId
                      modifyMVar_  ref $ \mr -> return $ case mr of
--                                   Nothing ->Just $ EventNode (id,th,False,Just $ toDyn r,ref')
                                   Just(EventNode(i,_,ins,_,ref)) -> Just $ EventNode(i,th,False,Just $ unsafeCoerce r,ref)
                      case cont' of
                        EventF  x f _ _ _ _ _-> do
                          mr <- runStateT  (runTrans x) cont'  !> "runx"
                          case mr of
                            (Nothing,_) ->
                               modifyMVar_ ref $
                                   \(Just(EventNode(i,th,ins,rec,ref))) -> return $ Just $ EventNode(i,th,True,rec,ref)


                            (Just r,cont'') ->do
--                               th <- myThreadId
                               let ins = inspected cont''

                               modifyMVar_ ref $
                                 \(Just(EventNode(i,th,ins,_,ref))) -> return $ Just $ EventNode(i,th,False,Nothing,ref)

                               delEvents ref'  ins    -- !> ("delEvents, activated    "++ show th)

                               runStateT (runTrans $ ( compose $ unsafeCoerce f) r) cont''
                               return ()
--                     delEvents ref' []

                   modifyMVar_  ref $ \ (Just(EventNode(i,_,ins,v,ref))) -> return $ Just $ EventNode (i,th',ins,v,ref)
                   return  $ cont{inspected= id:inspected  cont,eventHandlers=ref'}

          put mcont  -- !> ("return " ++ show (eventHandlers cont))

          return Nothing 

        False -> do
          put cont{inspected= id:inspected  cont} !> ("TRUE node=" ++ show rootRef)

          th' <- liftIO myThreadId

          if inspectedValue== False && th== th' && isJust mrec then do
               return $ Just $ unsafeCoerce $ fromJust mrec

          else if inspectedValue == True && isJust mrec then
               return $ Just $ unsafeCoerce $ fromJust mrec

          else return Nothing


        where


        loop Once rec x  = rec >>= x
        loop Loop rec f = do
            r <- rec
            f r
            loop Loop rec f

        loop Multithread rec f = do
            r <- rec
            forkIO $ f r
            loop Multithread rec f

        lookTree :: EventId -> MVar (Maybe EventNode) -> IO EventNode
        lookTree id ref=  do
            mn <- readMVar ref
            case mn of
              Nothing -> return emptyNode
              Just (node@(EventNode(id',_,_,_,ref'))) ->if id== id' then return node else  lookTree id ref'

--        delEvents :: IORef EventTree  -> IO()
        delEvents ref exc= do
            mevs <- readMVar ref   !> ("toDelete="++ show ref) !> ("exc="++ show exc)
            todel <- case mevs of
              Nothing ->  return False
              Just (EventNode (id,th,_,_,ref')) ->
                  if id `elem` exc then return False
                                   else killThread th >> delEvents ref' exc >> return True

            when todel $ modifyMVar_ ref $ const $ return $ Nothing



hash f= liftIO $ do
          st <- makeStableName $! f `seq` f
          return $hashStableName st

uhash= unsafePerformIO .hash
             
getLineRef= unsafePerformIO $ newTVarIO Nothing


option1 x  message=  inputLoop `seq` (waitEvents  $ do
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
            _ -> retry)
     where
     reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else readsPrec 0 s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined

option ret message= do
    liftIO $ putStrLn $"Enter ("++show ret++")" ++ message
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
           r<- getLine         !> "started inputLoop"
           if r=="end" then return True else do
              atomically . writeTVar  getLineRef $ Just r
              inputLoop

--inputLoop= unsafePerformIO $! forkIO $ inputLoop'
--       where
--       inputLoop'= do
--           r<- getLine
--           atomically . writeTVar  getLineRef $ Just r
--           inputLoop'
