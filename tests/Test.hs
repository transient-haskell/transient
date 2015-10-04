{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}

module Main where
import Transient.Base
import Transient.Move
import Transient.Logged
import Transient.DDS
import Control.Applicative
import System.Random
import Control.Monad.IO.Class

import System.IO
import Control.Monad
import Data.Monoid

import Data.Typeable
import Data.List
import Control.Exception
--import System.Environment
import System.Directory

main= do
 do
   createDirectoryIfMissing True "DDS"
   let numNodes= 5
       ports= [2000.. 2000+ numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports
--   args <- getArgs
--   let ports= [ 2000,  2001]
--
--   let [port1, port2]= if null args  then ports else reverse ports
--       local= "localhost"
--   print [port1, port2]
--   let mynode= createNode local port1
--   let node = createNode local port2
--   let nodes= [mynode,node]
--   putStrl "press ENTER to start"
--   getChar

   addNodes nodes
   keep $  do
        foldl (<|>) empty (map listen nodes) <|> return()


        let cdata = distribute [1..10000  :: Int]

        let cdata' = cmap (*2) cdata
        r <- reduce (+) cdata'
        liftIO $ print r
        exit


