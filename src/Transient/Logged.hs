-----------------------------------------------------------------------------
--
-- Module      :  Transient.Logged
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE  ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
module Transient.Logged  where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base
import Transient.Internals(onNothing,IDynamic(..),Log(..),LogElem(..),RemoteStatus(..),StateIO)
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef


class (Show a, Read a,Typeable a) => Loggable a
instance (Show a, Read a,Typeable a) => Loggable a

fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)=r where r= unsafeCoerce x     -- !> "coerce" ++ " to type "++ show (typeOf r)

fromIDyn (IDyns s)=r `seq`r where r= read s         -- !> "read " ++ s ++ " to type "++ show (typeOf r)

toIDyn x= IDynamic x

{- TODO add save/recover from log
rerun :: Log -> TransIO a -> TransIO a

getLog :: TransIO Log
-}

-- | write the result of the computation in  the log and return it.
-- but if there is data in the internal log, it read the data from the log and
-- do not execute the computation.
--
-- It accept nested step's. The effect is that if the outer step is executed completely
-- the log of the inner steps are erased. If it is not the case, the inner steps are logged
-- this reduce the log of large computations to the minimum. That is a feature not present
-- in the package Workflow.
--
-- >  r <- logged $ do
-- >          logged this :: TransIO ()
-- >          logged that :: TransIO ()
-- >          logged thatOther
-- >  liftIO $ print r
--
--  when `print` is executed, the log is just the value of r.
--
--  but at the `thatOther` execution the log is: [Exec,(), ()]
--
logged :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
logged mx =  Transient $ do
   Log recover rs full <- getData `onNothing` return ( Log False  [][])
   runTrans $
    case (recover ,rs) of        -- !> ("logged enter",recover,rs) of
      (True, Var x: rs') -> do
            setData $ Log True rs' full
            return $ fromIDyn x
--                                   !> ("read in Var:", x)

      (True, Exec:rs') -> do
            setData $ Log True  rs' full
            mx                                  -- !> "Exec"

      (True, Wait:rs') -> do
            setData (Log True  rs' full)        -- !> "Wait"
            empty

      _ -> do
--            let add= Exec: full
            setData $ Log False (Exec : rs) (Exec: full)     -- !> ("setLog False", Exec:rs)

            r <-  mx <*** ( do  -- when   p1 <|> p2, to avoid the re-execution of p1 at the
                                -- recovery when p1 is asynchronous
                            r <- getSData <|> return NoRemote
                            case r of
                                      WasParallel ->
--                                         let add= Wait: full
                                           setData $ Log False (Wait: rs) (Wait: full)
                                      _ -> return ())

            Log recoverAfter lognew _ <- getData `onNothing` return ( Log False  [][])
            let add= Var (toIDyn r):  full
            if recoverAfter && (not $ null lognew)      -- !> ("recoverAfter", recoverAfter)
              then  (setData $ Log True lognew (reverse lognew ++ add) )
                                                        -- !> ("recover",reverse lognew ,add)
              else if recoverAfter && (null lognew) then
                   setData $ Log False [] add
              else
                  (setData $ Log False (Var (toIDyn r):rs) add)  -- !> ("restore", (Var (toIDyn r):rs))
            return  r



