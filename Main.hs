module Main where

import Base
import Control.Monad.State.Strict
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic



threshold = 100

main= (flip runStateT) eventf0 $ do
  runTrans  $ do
     pr <- profits
     liftIO $ if pr > threshold
         then  putStr "Passed threshold!.. mail sent to boss: " >> print pr
         else  print pr
   
  eventLoop eventList
  
  liftIO $ putStrLn "END"
  




eventList=[Event "quantity" $ int2Dyn 10
                ,Event "price" $ int2Dyn 2
                ,Event "price" $ int2Dyn 3
          ,Event "quantity" $ int2Dyn 30
                ,Event "price" $ int2Dyn 4]

  where
  int2Dyn :: Int -> Dynamic
  int2Dyn= toDyn
  
-- show

profits :: TransientIO Int
profits= do
      quantity <-   waitEvent "quantity"
      liftIO $ do
            putStr "quantity="
            print (quantity :: Int)
            getChar
      price <-   waitEvent "price"
      liftIO $ do
            putStr "price="
            print price
            getChar
      let total= quantity * price
      liftIO $ do
         putStr $ "total="
         print total
         getChar
      return total

-- /show

profits' :: TransientIO Int
profits'=  do
    total<- (*) <$> currentEventValue "quantity" <*> currentEventValue "price"
    liftIO $ do
         putStr $ "total="
         print $ total
         getChar
         
    return total
