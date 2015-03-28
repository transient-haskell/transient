-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability : |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification,FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses #-}

-- show
module Base  where 
-- /show

import Control.Monad.State
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic

import Data.Monoid

--import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Conc
import Data.Maybe
-- import Debug.Trace


(!>) =  const . id -- flip trace
infixr 0 !>

data Transient m x= Transient  {runTrans :: m (Maybe x)}
type SData= ()

type EventId= Int



data EventF  = forall a b . EventF{closure :: TransientIO a
                                  ,continuation :: a -> TransientIO b
                                  ,mfData :: M.Map TypeRep SData
                                  ,mfSequence :: Int
                                  ,row :: P RowElem
                                  ,replay :: Bool
                                  }

type P= MVar

type Buffer= Maybe ()
type NodeTuple= (EventId, ThreadId, Buffer)

type Children=  Maybe (P RowElem)

data RowElem=   Node NodeTuple |  RowList Row Children

instance Show RowElem where
  show (Node (e,_,_))= show e
  show (RowList r ch)= show ( reverse r)  ++ "->" ++ show ch

type Row = [P RowElem]

instance Eq NodeTuple where
     (i,_,_) ==  (i',_,_)= i == i'


instance Show x => Show (MVar x) where
  show  x = show (unsafePerformIO $ readMVar x)

eventf0= EventF  empty (const  empty) M.empty 0
          rootRef False

-- {-# NOINLINE topNode #-}
-- topNode= (-1 :: Int,unsafePerformIO $ myThreadId,False,Nothing)

{-# NOINLINE rootRef #-}
rootRef :: MVar RowElem
rootRef=  unsafePerformIO $ newMVar $ RowList []  Nothing                      

instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

--runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
--runTrans (Transient mx) = mx

runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= runStateT (runTrans t) eventf0


newRow :: MonadIO m => m (P RowElem)
newRow= liftIO $ newMVar $ RowList [] Nothing

setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF   _ fs d _  ro r)  <- get
   n <- if replay st then return $ mfSequence st
     else  liftIO $ readMVar refSequence
   ro' <- newRow
   ro `eat` ro'
   put $ EventF   x ( \x -> f x >>= unsafeCoerce fs) d n  ro' r !> ("stored " ++ show n)
   return st

eat ro ro'= liftIO $
 modifyMVar_  ro $ \(RowList es t) -> return $ RowList (ro':es) t

resetEventCont (EventF x fs _ _  _ _)=do
   st@(EventF   _ _ d  n  ro r )  <- get
   put $ EventF  x fs d n  ro r


getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF  x fs _ _  _ _)= do runIt  x (unsafeCoerce fs); return ()
   where
   runIt  x fs= runTrans $ do
         st <- get
         --put st{mfSequence=i}
         r <- x
         put st
         fs r


runClosure :: EventF -> StateIO (Maybe a)
runClosure (EventF x _ _ _ _ _) =  unsafeCoerce $ runTrans x

runContinuation ::  EventF -> a -> StateIO (Maybe b)
runContinuation (EventF _ fs _ _ _ _ ) x= runTrans $  (unsafeCoerce fs) x 

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
           Just k  -> do addDescent' !> "ADDROW" ; runTrans $ f k

           Nothing -> return Nothing

        where
        addDescent'= do
            r <- gets row
            n <- addDescent r
            modify $ \s -> s{row= n}
addDescent r=
            liftIO $ do
              n <- newMVar $ RowList [] Nothing
              modifyMVar_ r $ \(RowList ns ch) ->  return $ RowList  ns $ Just n
             --   case ch of
             --     Just x -> error $ "children not empty: "++ show x
             --     Nothing ->  return $ RowList  ns $ Just n
              return n

addChild row ref= modifyMVar_  row $ \(RowList ns t) -> return $  RowList (ref : ns) t
   
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

{-# NOINLINE refSequence #-}
refSequence :: MVar Int
refSequence= unsafePerformIO $ newMVar 0


--- IO events


data Loop= Once | Loop | Multithread deriving Eq

waitEvents ::  IO b -> TransientIO b
waitEvents= parallel Loop


async  :: IO b -> TransientIO b
async = parallel Once

spawn= parallel Multithread

parallel  ::  Loop ->  IO b -> TransientIO b
parallel hasloop receive =  Transient $ do
      cont <- getCont
      id <- genNewId
      liftIO $ forkCont id hasloop receive cont

forkCont::  EventId -> Loop -> IO a -> EventF -> IO (Maybe a)
forkCont id hasloop receive cont= do
      let currentRow= row cont
      mnode  <-   liftIO $ lookTree id currentRow !> ("idToLook="++ show id++ " in: "++ show currentRow)
      
      case mnode of
        Nothing ->do
                 return () !> "NOT FOUND"
                 forkCont' id cont hasloop receive
                 return Nothing
        
        Just (node@(id',th', mrec)) -> do
         -- modify $ \cont -> cont{nodeInfo=Nothing}
          return $ if isJust mrec then Just $ unsafeCoerce $ fromJust mrec else Nothing

        where
        forkCont' id cont hasloop receive= liftIO $ forkIO $ do
                     th <- myThreadId
                     ref <-newMVar  $  Node (id,th,Nothing)
                     addChild (row cont) ref

                     loop hasloop  receive $ \r -> do
                       modifyMVar_  ref $ \(Node(i,th,_)) -> return
                                       $ Node(i,th,Just $ unsafeCoerce r)
                       (flip runStateT) cont $ do
                           cont@(EventF  x fs _  _ _ _) <- get
                           
                           put cont{replay= True{-,-mfSequence=i,-}{-nodeInfo=Just ref-}}
                           
                           mr <- runClosure cont 
                           case mr  of
                             Nothing -> return Nothing
                             Just r  -> do
                               row1 <- gets row 
                               liftIO $ delEvents  row1              !> ("delEvents: "++ show row1)
                               id <- liftIO $ readMVar refSequence
                               n <-  addDescent  row1
                               modify $ \cont -> cont{row=n,replay= False,mfSequence=id } !> ("SEQ=" ++ show(mfSequence cont))
                               runContinuation cont r
                       return ()


        
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
            RowList ns _<- readMVar ref
            lookList id ns



        lookList id mn= case mn of
              [] -> return Nothing
              (p:nodes) -> do
                  me <- readMVar p
                  case me of
                    Node(node@((id',_,_))) ->
                      if id== id'
                         then return $ Just node
                         else lookList id nodes
                    RowList row _ -> do
                         mx <- lookList id nodes
                         case mx of
                           Nothing -> lookList id row
                           Just x -> return $ Just x
        delEvents :: P RowElem  -> IO()
        delEvents ref = do
            RowList mevs mch <- takeMVar ref
            maybeDel mch
            putMVar ref $ RowList mevs Nothing

        maybeDel mch=  case mch of
              Nothing -> return ()
              Just p -> do
                  RowList es mch' <- readMVar p
                  delList es !> ("toDelete="++ show es)
                  maybeDel mch'


        delList es=  mapM_ del es where
          del p = readMVar p >>= del'
          del' (Node(node@(_,th,_)))= killThread th !> ("DELETING " ++ show node)
          del' (RowList l mch)= delList l >> maybeDel mch


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


{-# NOINLINE getLineRef #-}        
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

           