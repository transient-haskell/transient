{-# LANGUAGE   CPP, ScopedTypeVariables #-}



import Transient.Base
import Transient.EVars
import Transient.Backtrack
import Transient.Indeterminism
import Transient.Internals
import Transient.Logged

import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class


import Control.Exception

suspend :: a -> TransIO a
suspend file x= do
   Log recovery _ log <- getData `onNothing` return (Log False [] [])
   if recovery then return x else do
        mlogfile <- getData

        newlogfile <- liftIO $ fmap ("log" ++) <$> randName
        case mlogfile of
           Just (LogFile _ prev) -> do
             removeFile prev `catch` (\(e::SomeException) -> return ())
             setData $ Logfile (Just prev) logfilw
           Nothing -> do
             setSData $ LogFile Nothing $ logfile

        liftIO $ writeFile newlogfile $ show log
        exit x

checkpoint :: String -> TransIO ()
checkpoint file= do
   Log recovery _ log <- getData  `onNothing` return (Log False [] [])
   if recovery then return () else do
        liftIO $ appendFile file  $ show log
        return ()

type Restore=
file0 file1...
usar arbol de threads: parent
 logear con  (threadId parent log): un fichero cada uno
                 nombre/key: parent:thread
 restore: buscar el que tiene como parent Nothing
           buscar los que tienen como parent su threadId
           hasta que no hay ninguno: ese log es el que hay que usar
           ejecutar esa serie de logs resultantes
      Problema: repeticiones
         Solucion: al salvar, buscar los parents y calcular su longitud
      Problema: theadIds no validos para mas de una ejecuci¢n
         borrar cada serie de logs y reescribirlo con los nuevos threadIds
         Problema: repeticiones

restore :: String -> TransIO a -> TransIO a
restore pathLogs  proc= do
     list <- getDirectoryContents pathLogs
     log <- choose list
     liftIO $ print log
     setData $ Log True (reverse log) log
     proc

main= keep $ restore "files" $ do
     r <- logged $ choose [1..10 :: Int]
     suspend  ()
     logged $ liftIO $ print ("hello",r)

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
