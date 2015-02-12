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

(!>) = const . id --    flip trace
infixr 0 !>

data Transient m x= Transient  {runTrans :: m (Maybe x)}
type SData= ()

type EventId= Int

data EventF  = forall a b . EventF{xcomp :: (EventId,P Elem,TransientIO a)
                                  ,fcomp :: [a -> TransientIO b]
                                  ,mfData :: M.Map TypeRep SData
                                  ,mfSequence :: Int
                                  ,nodeInfo :: Maybe (P Elem)
                                  ,row :: P Elem
                                  ,replay :: Bool
                                  }

type P= MVar

(=:) :: P a  -> a -> IO()
(=:) n v= modifyMVar_ n $ const $ return v

type Inspected= Bool
type Buffer= Maybe Dynamic
type NodeTuple= (EventId, ThreadId,Bool, Buffer)
type Row = [P Elem]
type Children=  Maybe  (P Elem)

data Elem=   Node NodeTuple |  List Row Children



instance Show Elem where
  show (Node (e,_,_,_))= show e
  show (List r t )= "<" ++ show1 ( reverse r) ++ ":"++
   case t of
     Nothing -> ""
     Just ch -> show ch
   ++">"

show1 xs=  concatMap ((++ " "). show) xs



newRow :: MonadIO m => m (P Elem)
newRow= liftIO $ newMVar $ List [] Nothing

instance Eq NodeTuple where
     (i,_,_,_) ==  (i',_,_,_)= i == i'


instance Show x => Show (MVar x) where
  show  x =  show (unsafePerformIO $ readMVar x)

eventf0= EventF  (-1,unsafePerformIO $ newMVar $ List [] Nothing,empty)
                 [const $ empty] M.empty 0
         Nothing  rootRef False


--topNode= (-1 :: Int,unsafePerformIO $ myThreadId,False,Nothing)
--
rootRef :: MVar Elem
rootRef=  unsafePerformIO $ newMVar $ List [] Nothing                      

instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO



runTransient :: TransientIO x -> IO (Maybe x, EventF)
runTransient t= runStateT (runTrans t) eventf0

setEventCont ::   TransientIO a -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF   (_,_,_) fs d _ es ro r)  <- get
   n <- if replay st then return $ mfSequence st
     else  liftIO $ readMVar refSequence
   ro' <- newRow
   ro `eat` ro'
   put $ EventF   (n,ro,x) (f: unsafeCoerce fs) d n es ro'  r !> ("stored " ++ show n)
   return st

eat ro ro'= liftIO $
 modifyMVar_  ro $ \(List es t) -> return $ List (ro':es) t

              
resetEventCont (EventF x fs _ _ _ _ _)=do
   st@(EventF   _ _ d  n es ro  r )  <- get
   put $ EventF  x fs d n es ro r


getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF  (i,_,x) fs _ _ _ _ _)= do runIt i x (unsafeCoerce fs); return ()
   where 
   runIt i x fs= runTrans $ do
         st <- get
         put st{mfSequence=i}
         r <- x
         put st
         compose fs r
--         where
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
           Just k  ->  runTrans $ f k

           Nothing -> return Nothing
        where
--        addRow'= do
--            r <- gets row
--            n <- addRow r
--            modify $ \s -> s{row= n}
addRow r=
            liftIO $ do
              n <- newMVar $ List [] Nothing
              modifyMVar_ r $ \(List ns _) ->   return $ List  ns $ Just n
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

data Loop= Once | Loop | Multithread deriving Eq

waitEvents ::  Typeable b => IO b -> TransientIO b
waitEvents= parallel Loop


async  :: Typeable b => IO b -> TransientIO b
async = parallel Once

parallel  :: Typeable b => Loop ->  IO b -> TransientIO b
parallel hasloop receive =  Transient $ do

      cont <- getCont
      id <- genNewId
      let currentRow= row cont
--      let  mnode=  nodeInfo cont
      started  <-   liftIO $ lookTree id currentRow !> ("idToLook="++ show id++ " in: "++ show currentRow)
      
      case started of
        Nothing ->do
                 return () !> "NOT FOUND"
                 liftIO $ do
                   ref <- newMVar $ Node (id,undefined,False,Nothing)

                   modifyMVar_ (row cont) $ \(List ns t) -> return $  List (ref : ns) t
                   forkIO $ do
                     th <- myThreadId
                     modifyMVar_ ref $ \(Node(id,_,f,n)) -> return $ Node (id,th,f,n)


                     loop hasloop  receive $ \r -> do

                      th <-  myThreadId
                      modifyMVar_  ref $ \(Node(i,_,_,_)) -> return
                                       $ Node(i,th,False,Just $ toDyn r)
                      case cont of
                        EventF  (i,_,x) fs _ _ _ _ _-> do
                          mr <- runStateT  (runTrans x)
                                cont{replay= True,mfSequence=i,nodeInfo=Just ref}
                             !> "runx" !> ("mfSequence="++ show i)
                          case mr  of
                            (Nothing,_) -> return ()
--                               modifyMVar_ ref $ \(Node(i,th,ins,rec)) -> return
--                                               $ Node(i,th,True,rec)


                            (Just r,cont') -> do
                               modifyMVar_ ref $ \(Node(i,th,ins,mrec)) -> return
                                               $ Node (i,th,False,Nothing)
                               let row1 = row cont'
                               List r t <- readMVar row1
                               delEvents  t       !> ("delEvents    "++ show t)

                               id <- readMVar refSequence
                               nrow <- if hasloop==Multithread then return row1 else addRow row1

                               runStateT (runTrans $ ( compose $ unsafeCoerce fs) r)
                                       cont'{replay= False,mfSequence=id, row= nrow }
                               return () !> ("ROOT1=" ++ show rootRef) !> ("ROW"++ show row1)


                   modifyMVar_ (row cont) $ \(List ns ch) -> return $  List (ref : ns) ch


                 return Nothing 


        Just  (node@(id',th',inspectedValue, mrec))  -> do
          modify $ \cont -> cont{nodeInfo=Nothing} 
--          Node (node@(id',th',inspectedValue, mrec)) <- liftIO $ readMVar ref
          return () !>  "FOUND"  !> show node
          if isJust mrec then return $  fromDynamic$ fromJust mrec else return Nothing
--          th <- liftIO myThreadId
--
--          return () !>  "FOUND" !> show th !> show th' !> show inspectedValue !> show node
--
--          if inspectedValue== False && th== th' && isJust mrec then do
--               return $  fromDynamic$ fromJust mrec !> ("ROOT2=" ++ show rootRef)  !> ("ROW="++ show (row cont))
--
--          else if inspectedValue == True && isJust mrec then
--               return $  fromDynamic $ fromJust mrec !> ("ROOT3=" ++ show rootRef)  !> ("ROW="++ show (row cont))
--
--          else return Nothing


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

        taill Nothing= return Nothing
        taill(Just l)= readMVar l >>= \(List h t) -> return t

        delEvents Nothing = return ()



        delEvents (Just r)= do
              List h t <- readMVar r
              delList h

              delEvents t




        delList es=  mapM_ del es where
          del p = readMVar p >>= del'
          del' (Node(node@(_,th,_,_)))= killThread th !> ("DELETING " ++ show node ++ " "++show (length es))
          del' (List h t)= delList h>> delEvents t !> "rowkist="

        lookTree :: EventId -> P Elem -> IO (Maybe NodeTuple)
        lookTree id ref=  do
            List ns _<- readMVar ref
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
                    List row _ -> do
                         mx <- lookList id nodes
                         case mx of
                           Nothing -> lookList id row
                           Just x -> return $ Just x


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
    liftIO $ putStrLn $"Enter ("++show ret++")for:\t" ++ message
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
