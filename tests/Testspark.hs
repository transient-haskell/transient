{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable  #-}
module Main where
import Transient.Base
import Transient.Stream.Resource
import Data.Char
import Control.Monad.IO.Class

main= keep . threads 0  $ do
         chunk <- sourceFile "../transient.cabal"
         liftIO $ print chunk
         return $ map toUpper chunk
       `sinkFile` "outfile"

