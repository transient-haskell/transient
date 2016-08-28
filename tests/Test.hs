{-# LANGUAGE   CPP, ScopedTypeVariables, DeriveDataTypeable #-}



import Transient.Base
import Transient.EVars
import Transient.Backtrack
import Transient.Indeterminism
import Transient.Internals
import Transient.Logged
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import System.Directory
import System.Random
import Control.Exception


--logs= "logs/"
--
--suspend :: a -> TransIO a
--suspend  x= do
--   Log recovery _ log <- getData `onNothing` return (Log False [] [])
--   if recovery then return x else do
--        logAll  log
--        exit x
--
--checkpoint ::  TransIO ()
--checkpoint = do
--   Log recovery _ log <- getData `onNothing` return (Log False [] [])
--   if recovery then return () else logAll log
--
--
--logAll log= do
--
--        newlogfile <- liftIO $  (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
--        liftIO $ writeFile newlogfile $ show log
--      :: TransIO ()
--
--
--restore :: TransIO a -> TransIO a
--restore   proc= do
--     liftIO $ createDirectory logs  `catch` (\(e :: SomeException) -> return ())
--     list <- liftIO $ getDirectoryContents logs
--                 `catch` (\(e::SomeException) -> return [])
--     if length list== 2 then proc else do
--
--         let list'= filter ((/=) '.' . head) list
--         file <- choose  list'       -- !> list'
--
--         logstr <- liftIO $ readFile (logs++file)
--         let log= length logstr `seq` read' logstr
--
--         log `seq` setData (Log True (reverse log) log)
--         liftIO $ remove $ logs ++ file -- setData $ LogFile  file
--         proc
--     where
--     read'= fst . head . reads1
--
--     remove f=  removeFile f `catch` (\(e::SomeException) -> remove f)

main= keep $ restore  $ do
     r <- logged $ choose [1..10 :: Int]
     logged $ liftIO $ print ("hello",r)
     suspend ()
     logged $ liftIO $ print ("world",r)
     checkpoint
     logged $ liftIO $ print ("world22222",r)

main2= keep $ do
    ev <- newEVar

    readers ev 100 <|> writers ev 100



readers ev n= foldr (<>) mempty $ take n $ repeat $ readEVar ev

writers ev n= foldr (<|>) empty $ take n $ repeat $ do
     waitEvents $ threadDelay 1000000
     writeEVar ev (1::Int)
     empty

instance Monoid Int where
  mempty= 0
  mappend= (+)
