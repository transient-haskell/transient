module Main where

import Base3
import Control.Monad.State
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic

waitEvent name = Transient $ do
  st <- get !> "waitEvent"
  let evs = eventHandlers  st !> "waitEvent2"

  case  M.lookup name evs of
    Nothing ->  do
       put st{ eventHandlers=  M.insert name st  evs}
       return Nothing
    Just _ ->  do
       put st{ eventHandlers=  M.insert name st evs}
       eventValue name


--eventValue :: Typeable a => String -> m a
eventValue name =  do
   me <- gets currentEvent !> "eventValue"
   case me of
    Nothing -> return Nothing   !> "NO EVENT"
    Just (Event name' r) -> do
     if name /= name' then return Nothing  !> " eventValue: not the event" else do
      case fromDynamic r of
       Nothing -> return Nothing !> "eventValue: Nothing"
       Just x -> do liftIO $ putStrLn $ "read event: " ++ name
                    return $ Just x


threshold = 100


main= runStateT (do
  runTrans  $ do
       pr <- profits
       liftIO $ if pr > threshold
         then  putStr "Passed threshold!.. mail sent to boss " >> print pr
         else  print pr
  eventLoop eventList
  liftIO $ putStrLn "END")
  eventf0




eventList=[ Event "quantity" $ int2Dyn 10, Event "price" $ int2Dyn 2
                                         , Event "price" $ int2Dyn 3
          , Event "quantity" $ int2Dyn 30, Event "price" $ int2Dyn 4]

  where
  int2Dyn :: Int -> Dynamic
  int2Dyn= toDyn
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
      liftIO $ do
         putStr $ "total="
         print $ quantity * price
         getChar
      return $ quantity * price

-- /show
