-----------------------------------------------------------------------------
--
-- Module      :  Transient.Logged
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | The 'logged' primitive is used to save the results of the subcomputations
-- of a transient computation (including all its threads) in a log buffer. At
-- any point, a 'suspend' or 'checkpoint' can be used to save the accumulated
-- log on a persistent storage. A 'restore' reads the saved logs and resumes
-- the computation from the saved checkpoint. On resumption, the saved results
-- are used for the computations which have already been performed. The log
-- contains purely application level state, and is therefore independent of the
-- underlying machine architecture. The saved logs can be sent across the wire
-- to another machine and the computation can then be resumed on that machine.
-- We can also save the log to gather diagnostic information.
--
-- The following example illustrates the APIs. In its first run 'suspend' saves
-- the state in a directory named @logs@ and exits, in the second run it
-- resumes from that point and then stops at the 'checkpoint', in the third run
-- it resumes from the checkpoint and then finishes.
--
-- @
-- main= keep $ restore  $ do
--      r <- logged $ choose [1..10 :: Int]
--      logged $ liftIO $ print (\"A",r)
--      suspend ()
--      logged $ liftIO $ print (\"B",r)
--      checkpoint
--      liftIO $ print (\"C",r)
-- @
-----------------------------------------------------------------------------
{-# LANGUAGE  CPP, ExistentialQuantification, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Transient.Logged(
Loggable, logged, received, param,
#ifndef ghcjs_HOST_OS
 suspend, checkpoint, restore,
#endif
-- * low level
fromIDyn,maybeFromIDyn,toIDyn
) where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base

import Transient.Indeterminism(choose)
import Transient.Internals -- (onNothing,reads1,IDynamic(..),Log(..),LogElem(..),RemoteStatus(..),StateIO)
import Control.Applicative
import Control.Monad.State
import System.Directory
import Control.Exception 
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS

#ifndef ghcjs_HOST_OS
import System.Random
#endif



#ifndef ghcjs_HOST_OS
logs= "logs/"

-- | Reads the saved logs from the @logs@ subdirectory of the current
-- directory, restores the state of the computation from the logs, and runs the
-- computation.  The log files are removed after the state has been restored.
--
restore :: TransIO a -> TransIO a
restore   proc= do
     liftIO $ createDirectory logs  `catch` (\(e :: SomeException) -> return ())
     list <- liftIO $ getDirectoryContents logs
                 `catch` (\(e::SomeException) -> return [])
     if null list || length list== 2 then proc else do

         let list'= filter ((/=) '.' . head) list
         file <- choose  list'       -- !> list'

         logstr <- liftIO $ readFile (logs++file)
         let log= length logstr `seq` read' logstr

         log `seq` setData (Log True (reverse log) log)
         liftIO $ remove $ logs ++ file
         proc
     where
     read'= fst . head . reads1

     remove f=  removeFile f `catch` (\(e::SomeException) -> remove f)



-- | Saves the logged state of the current computation that has been
-- accumulated using 'logged', and then 'exit's using the passed parameter as
-- the exit code. Note that all the computations before a 'suspend' must be
-- 'logged' to have a consistent log state. The logs are saved in the @logs@
-- subdirectory of the current directory. Each thread's log is saved in a
-- separate file.
--
suspend :: Typeable a => a -> TransIO a
suspend  x= do
   Log recovery _ log _ <- getData `onNothing` return (Log False [] [] 0)
   if recovery then return x else do
        logAll  log
        exit x

-- | Saves the accumulated logs of the current computation, like 'suspend', but
-- does not exit.
checkpoint ::  TransIO ()
checkpoint = do
   Log recovery _ log _ <- getData `onNothing` return (Log False [] [] 0)
   if recovery then return () else logAll log


logAll log= liftIO $do
        newlogfile <- (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
        logsExist <- doesDirectoryExist logs
        when (not logsExist) $ createDirectory logs
        writeFile newlogfile $ show log
      -- :: TransIO ()

#endif

maybeFromIDyn :: Loggable a => IDynamic -> Maybe a
maybeFromIDyn (IDynamic x)=  r
   where
   r= if typeOf (Just x)  == typeOf r then Just $ unsafeCoerce x else Nothing 

maybeFromIDyn (IDyns s) = case reads s  of
                            [] -> Nothing 
                            [(x,"")] -> Just x 

fromIDyn :: Loggable a => IDynamic -> a
fromIDyn (IDynamic x)=r where r= unsafeCoerce x     -- !> "coerce" ++ " to type "++ show (typeOf r)

fromIDyn (IDyns s)=r `seq`r where r= read' s        --  !> "read " ++ s ++ " to type "++ show (typeOf r)



toIDyn x= IDynamic x



-- | Run the computation, write its result in a log in the parent computation
-- and return the result. If the log already contains the result of this
-- computation ('restore'd from previous saved state) then that result is used
-- instead of running the computation again.
--
-- 'logged' can be used for computations inside a 'logged' computation. Once
-- the parent computation is finished its internal (subcomputation) logs are
-- discarded.
--
logged :: Loggable a => TransIO a -> TransIO a
logged mx = Transient $  do
       Log recover rs full hash <- getData `onNothing` return ( Log False  [][] 0)
       runTrans $
        case (recover ,rs)    of                     --   !> ("logged enter",recover,rs,reverse full) of
          (True, Var x: rs') -> do
                return ()                            --      !> ("Var:", x)
                setData $ Log True rs' full (hash+ 10000000)
                return $ fromIDyn x
                                                   
    
          (True, Exec:rs') -> do
                setData $ Log True  rs' full (hash + 1000)
                mx                                    --    !> "Exec"
    
          (True, Wait:rs') -> do
                setData $ Log True  rs' full (hash + 100000)
                setData WasParallel
                empty                                 --  !> "Wait"
    
          _ -> do

                setData $ Log False (Exec : rs) (Exec: full)  (hash + 1000)     -- !> ("setLog False", Exec:rs)
    
                r <-  mx <** do setData $ Log False (Wait: rs) (Wait: full)  (hash+ 100000)
                                    -- when   p1 <|> p2, to avoid the re-execution of p1 at the
                                    -- recovery when p1 is asynchronous or return empty

                Log recoverAfter lognew _ _ <- getData `onNothing` return ( Log False  [][] 0)
                let add= Var (toIDyn r):  full
                if recoverAfter && (not $ null lognew)        --  !> ("recoverAfter", recoverAfter)
                  then  do
                    setData WasParallel
                    (setData $ Log True lognew (reverse lognew ++ add)  (hash + 10000000) )
                                                                          -- !> ("recover",reverse (reverse lognew ++add))
                  else if recoverAfter && (null lognew) then do 
                       -- showClosure
                       setData $ Log False [] add  (hash + 10000000)      --  !> ("recover2",reverse add)
                  else do
                      -- showClosure
                    (setData $ Log False (Var (toIDyn r):rs) add (hash +10000000))  --  !> ("restore", reverse $ (Var (toIDyn r):rs))
           
                return  r



-------- parsing the log for API's

received :: Loggable a => a -> TransIO ()
received n=Transient $  do

   Log recover rs full hash <- getData `onNothing` return ( Log False  [][] 0)
   return ("RECEIVED", rs)

   case rs of 
     [] -> return Nothing
     Var (IDyns s):t -> if s == show1 n
          then  do
            return() !> "valid"
            setData $ Log recover t full hash
            return $ Just ()
          else return Nothing
     _  -> return Nothing
   where
   show1 x= if typeOf x == typeOf "" then unsafeCoerce x 
            else if typeOf x== typeOf (undefined :: BS.ByteString) then unsafeCoerce x
            else if typeOf x== typeOf (undefined :: BSS.ByteString) then unsafeCoerce x
            else show x

param :: Loggable a => TransIO a
param= res where
 res= Transient $  do

   Log recover rs full hash<- getData `onNothing` return ( Log False  [][] 0) 
   return () !> ("PARAM",rs)
   case rs of

     [] -> return Nothing
     Var (IDynamic v):t ->do
           return () !> ("IDynnnnnnnnnnn", show v)
           setData $ Log recover t full hash
           return $ cast v
     Var (IDyns s):t -> do
       return () !> ("IDyn",s)
       let mr = reads1  s `asTypeOf` type1 res
       case mr of
          [] -> return Nothing
          ((v,r):_) -> do
              setData $ Log recover t full hash
              return $ Just v
     _ -> return Nothing

   where
   

   reads1 s=x where
      x= if typeOf(typeOfr x) == typeOf "" then unsafeCoerce[(s,"")] else reads s
      typeOfr :: [(a,String)] ->  a
      typeOfr  = undefined
   type1 :: TransIO a -> [(a,String)]
   type1= error "type1: typelevel"
