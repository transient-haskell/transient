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
import Control.Applicative
import Control.Monad.IO.Class

{-
newtype TransLIO  a =  TransLIO {runLogged :: TransIO a}

--data RLogged= forall a.(Read a, Show a) => RLogged  a

instance Functor TransLIO  where
--   fmap f mx=  mx >>= \(TransLIO x) ->  TransLIO (f x)

instance Applicative TransLIO where
--   pure= return
--   f <*> g= TransLIO $ do
--         x <- f
--         y <- g
--         return $ x y

instance  Monad TransLIO  where
   return  x=  TransLIO $ return x
   TransLIO x >>= f =  TransLIO $  do
         r <- x
         runLogged $ f r

-}

--data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

--instance Show IDynamic where
--  show (IDynamic x)= show $ show x
--  show (IDyns s)= show s
--
--instance Read IDynamic where
--  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str

class (Show a, Read a,Typeable a) => Loggable a
instance (Show a, Read a,Typeable a) => Loggable a

fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)=r where r= read s  -- !!> "read " ++ s ++ "to type "++ show (typeOf r)

toIDyn x= IDynamic x

-- | synonymous of `step`
logged :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
logged= step

step' mx act=  Transient $ do
   Log recover rs full <- getSessionData `onNothing` return ( Log False  [][])
   runTrans $
    case (recover,rs) of
      (True, Step x: rs') -> do
            setSData $ Log True rs' full
            (return $ fromIDyn x)              -- !!>  "read in step:" ++ show x

      (True, Exec:rs') -> do
            setSData $ Log True  rs' full
            mx                                 -- !!> "step True Exec"

      (True, WaitRemote:rs') -> do
            setSData (Log True  rs' full)      -- !!> "waitRemote2"
            empty

      (True, Wormhole:rs') -> do
            setSData (Log True  rs' full)      -- !!> "waitRemote2"
            mx

      _ -> act full mx






-- | write the result of the computation in  the log and return it.
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
step mx = step' mx $ \full mx -> do
            let add= Exec: full
            setSData $ Log False add add

            r <-  mx

            let add= Step (toIDyn r): full
            (setSData $ Log False add add)     -- !!> "AFTER STEP"
            return  r





{-
necesario indicar Exec/ByPass
  Exec - ejecutar
  PassTrough - ejecutar argumento
  Skip - WaitRemote
  Step

alternativa: a¤adir un tag especial para wormhole y detectar que es el ultimo

tres estados:
local- Exec remoto -> PassTrough -
step

exigiria cambiar de Exec a passtrough en fullLog como?
a¤adir tag Wormhole
-}
