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



--type Recover= Bool

--data LogElem=  WaitRemote | Exec | Step IDynamic deriving (Read,Show)

--type CurrentPointer= [LogElem]
--type LogEntries= [LogElem]

--data Log= Log Recover  CurrentPointer LogEntries deriving Typeable

step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
step mx=  do
    Log recover rs full <- getSData <|> return ( Log False  [][])
----    liftIO $ print rs
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
