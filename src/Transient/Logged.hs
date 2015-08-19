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
{-# LANGUAGE  ExistentialQuantification #-}
module Transient.Logged  where

import Data.Typeable
import Unsafe.Coerce
import Transient.Base
import Control.Applicative
import Control.Monad.IO.Class

--data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

--instance Show IDynamic where
--  show (IDynamic x)= show $ show x
--  show (IDyns s)= show s
--
--instance Read IDynamic where
--  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str


fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)=r where r= read s !> "read " ++ s ++ "to type "++ show (typeOf r)

toIDyn x= IDynamic x

-- | synonymous of `step`
logged :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
logged= step



-- | write the result of the computation in the log and return it.
-- but if there is data in the internal log, it read the data from the log and
-- do not execute the computation.
--
-- It accept nested step's. The effect is that if the outer step is executed completely
-- the log of the inner steps are erased. If it is not the case, the inner steps are logged
-- this reduce the log of large computations to the minimum. That is a feature not present
-- in the package Workflow.
--
-- >  r <- step $ do
-- >          step this :: TransIO ()
-- >          step that :: TransIO ()
-- >          step thatOther
-- >  liftIO $ print r
--
--  when `print` is executed, the log is just the value of r.
--
--  but when `thatOther` is executed the log is: [Exec,(), ()]
--
step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
step mx=  do
    Log recover rs full <- getSData <|> return ( Log False  [][])

    case (recover,rs) of
      (True, Step x: rs') -> do
            setSData $ Log recover rs' full
            return $ fromIDyn x  !>  "read in step:" ++ show x

      (True,Exec:rs') -> do
            setSData $ Log recover rs' full
            mx

      (True, WaitRemote:rs') -> do
            setSData (Log recover rs' full) !> "waitRemote2"
            empty

      _ -> do
            let add= Exec:  full
            setSData $ Log False add add
            r <-  mx
            let add= Step (toIDyn r): full
            setSData $ Log False add add
            return  r

--waitRemote mx= do
--    Log recover rs full <- getSData <|> return ( Log False  [][])
--
--    case (recover,rs) of
--      (True, Step x: rs') -> do
--            setSData $ Log recover rs' full
--            return $ fromIDyn x -- !>  "read in step:" ++ show x
--
--      (True,[WaitRemote]) -> do
--            setSData $ Log recover [] full !> "waitRemote1"
--            mx
--
--
--
--      (True, WaitRemote:rs') -> do
--            setSData $ Log recover rs' full !> "waitRemote2"
--
--            empty -- return $ error "not evaluable: waiting for remote result in Alternative/Applicative expression"
--
--
--
--      _ -> do
--            let add= WaitRemote:  full
--            setSData $ Log False add add   !> "waitRemote3"
--            r <-  mx
--            let add= Step (toIDyn r): full
--            setSData $ Log False add add
--            return  r
