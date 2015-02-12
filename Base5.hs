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

data EventF  = forall a b . EventF{xcomp :: (EventId,P RowElem,TransientIO a)
                                  ,fcomp :: [a -> TransientIO b]
                                  ,mfData :: M.Map TypeRep SData
                                  ,mfSequence :: Int
                                  ,nodeInfo :: P NodeTuple
                                  ,row :: P RowElem
                                  ,replay :: Bool
                                  }

type P= MVar

(=:) :: P a  -> a -> IO()
(=:) n v= modifyMVar_ n $ const $ return v

type Inspected= Bool
type Buffer= Maybe ()
type NodeTuple= (EventId, ThreadId,Bool, Buffer)

data RowElem=   Node NodeTuple |  RowList Row

instance Show RowElem where
  show (Node (e,_,_,_))= show e
  show (RowList r)= show $ reverse r

type Row = [P RowElem]

instance Eq NodeTuple where
     (i,_,_,_) ==  (i',_,_,_)= i == i'


instance Show x => Show (MVar x) where
  show  x = show (unsafePerformIO $ readMVar x)

eventf0= EventF  (-1,rootRef,empty) [const $ empty] M.empty 0
        (unsafePerformIO $ newMVar topNode) rootRef False


topNode= (-1,unsafePerformIO $ myThreadId,False,Nothing)

rootRef :: MVar RowElem
rootRef=  unsafePerformIO $ newMVar $ RowList []                        

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
   st@(EventF   _ fs d _ es ro r)  <- get
   n <- if replay st then return $ mfSequence st
     else  liftIO $ readMVar refSequence
   put $ EventF   (n,ro,x) ( f: unsafeCoerce fs) d n es ro r !> ("stored " ++ show n)
   return st

resetEventCont (EventF x fs _ _ _ _ _)=do
   st@(EventF   _ _ d  n es ro r )  <- get
   put $ EventF  x fs d n es ro r


getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF  (i,r,x) fs _ _ _ _ _)= do runIt i x (unsafeCoerce fs); return ()
   where
   runIt i x fs= runTrans $ do
         st <- get
         put st{mfSequence=i,row=r}
         r <- x
         put st
         compose fs r

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
      x >>= f  = Transient $ do
        cont <- setEventCont x  f
        mk <- runTrans x
        resetEventCont cont
        
        case mk of
           Just k  -> do addRow' !> "ADDROW" ; runTrans $ f k

           Nothing -> return Nothing

        where
        addRow'= do
            r <- gets row
            n <- addRow r
            modify $ \s -> s{row= n}
addRow r=
            liftIO $ do
              n <- newMVar $ RowList []
              modifyMVar_ r $ \(RowList ns) -> return $ RowList $  n:ns
              return n


   
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

      cont <- getCont
      id <- genNewId
      let currentRow= row cont

      mnode  <-   liftIO $ lookTree id currentRow !> ("idToLook="++ show id++ " in: "++ show currentRow)
      
      case mnode of
        Nothing ->do
                 return () !> "NOT FOUND"
                 liftIO $ do
                   ref <- newMVar $ Node (id,undefined,False,Nothing)
                   forkIO $ do
                     th <- myThreadId
                     modifyMVar_ ref $ \(Node(id,_,f,n)) -> return $ Node (id,th,f,n)


                     loop hasloop  receive $ \r -> do
                      th <-  myThreadId
                      modifyMVar_  ref $ \(Node(i,_,ins,_)) -> return
                                       $ Node(i,th,False,Just $ unsafeCoerce r)
                      case cont of
                        EventF  (i,row1,x) f _ _ _ _ _-> do
                          mr <- runStateT  (runTrans x)
                                cont{replay= True,mfSequence=i,row =row1}
                             !> "runx" !> ("mfSequence="++ show i)
                          case mr  of
                            (Nothing,_) ->
                               trace "NOTHING" $ modifyMVar_ ref $ \(Node(i,th,ins,rec)) -> return
                                               $ Node(i,th,True,rec)


                            (Just r,cont') ->do
                               modifyMVar_ ref $ \(Node(i,th,ins,_)) -> return
                                               $Node (i,th,False,Nothing)

                               delEvents  row1        !> ("delEvents, activated    "++ show th)
                               id <- readMVar refSequence
                               n <- addRow $ row cont'
                               runStateT (runTrans $ ( compose $ unsafeCoerce f) r)
                                       cont'{row=n,replay= False,mfSequence=id } !> ("SEQ=" ++ show(mfSequence cont'))
                               return ()
--                      delEvents children []


                   modifyMVar_ (row cont) $ \(RowList ns) -> return $  RowList $ ref : ns

                 return Nothing 

        Just node@(id',th',inspectedValue, mrec)-> do

          th <- liftIO myThreadId

          return () !>  "FOUND" !> show th !> show th' !> show inspectedValue !> show mrec

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

        lookTree :: EventId -> P RowElem -> IO (Maybe NodeTuple)
        lookTree id ref=  do
            RowList ns <- readMVar ref
            lookList id ns



        lookList id mn= case mn of
              [] -> return Nothing
              (p:nodes) -> do
                  me <- readMVar p
                  case me of
                    Node(node@((id',_,_,_))) ->
                      if id== id'
                         then return $ Just node
                         else lookList id nodes
                    RowList row -> do
                         mx <- lookList id nodes
                         case mx of
                           Nothing -> lookList id row
                           Just x -> return $ Just x

        delEvents :: P RowElem  -> IO()
        delEvents ref = do
            RowList mevs <- takeMVar ref
            l' <- delList'   mevs !> ("toDelete="++ show mevs)
            putMVar ref $ RowList l'


        delList' es= return . catMaybes =<< mapM del es
          where
          del p = readMVar p >>= del'
           where
           del' (node@(Node(_,th,_,_)))= return $ Just p  !> ("NOT DELETING " ++ show node)
           del' (RowList l)= delList l >> return Nothing

        delList es=  mapM_ del es where
          del p = readMVar p >>= del'
          del' (Node(node@(_,th,_,_)))= killThread th !> ("DELETING " ++ show node)
          del' (RowList l)= delList l



react
  :: Typeable eventdata
  => ((eventdata ->  IO response) -> IO ())
  -> (eventdata -> IO response)
  -> TransientIO eventdata

react setHandler iob= Transient $ do
        cont    <- getCont
        mEvData <- getSessionData
        case mEvData of
          Nothing -> do
            liftIO $ setHandler $ \dat ->do
--              let cont'= cont{mfData = M.insert (typeOf dat)(unsafeCoerce dat) (mfData cont)}
              runStateT (setSData dat >> runCont cont) cont
              iob dat
            return Nothing
          Just dat -> delSessionData dat >> return (Just  dat)

--hash f= liftIO $ do
--          st <- makeStableName $! f `seq` f
--          return $hashStableName st

--uhash= unsafePerformIO .hash
             
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
