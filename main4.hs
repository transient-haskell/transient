{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Basefail
import Control.Monad.State
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic

import Control.Concurrent.MVar
import Control.Concurrent
import Data.Monoid
import System.Random

import Data.IORef

threshold = 100


--main2= runStateT (do
--  runTrans  $ do
--       pr <- profits
--       liftIO $ if pr > threshold
--         then  putStr "Passed threshold!.. mail sent to boss " >> print pr
--         else  print pr
--  eventLoop eventList
--  liftIO $ putStrLn "END")
--  eventf0

runTransient t= runStateT (runTrans t) eventf0

main2=do
   runTransient $ do
     liftIO $ putStrLn "Enter your name:"
     name  <- waitEvents  getLine
     name' <- waitEvents  getLine
     liftIO $ putStrLn $ "hello "++ name ++ name'
   stay



main= do
    runTransient choose
    stay

data Option= Option String String | Number Int deriving Show

choose= do
--   r <- waitStop  <|> return False
--   if r
--     then do
--       liftIO $ print "STOP"
--       return ()
--     else do
       r <-   ((,) <$> async False ( return "1")  <*> async False (threadDelay 10000 >> return "2")) --  <|> return ("3","3")
             -- <|> (Number <$>  (return 3) )
       liftIO $ putStrLn $ "result=" ++ show r

       liftIO $print rootRef;


  where
  waitnumber= waitEvents  $ do
      vr <- newEmptyMVar
      forkIO $ threadDelay 5000000 >> putMVar vr  (12 ::Int)
      r <- takeMVar vr
      return r

  waitStop = do
      waitEvents $ getLine' (=="s")
      return True !> "STOP"
  waitContinue = do
      waitEvents $ getLine' (=="c")
      return True !> "STOP"


stay= takeMVar $ unsafePerformIO $ (newEmptyMVar :: IO (MVar()))

eventList=[ Event "quantity" $ int2Dyn 10, Event "price" $ int2Dyn 2
          , Event "price" $ int2Dyn 3
          , Event "quantity" $ int2Dyn 30, Event "price" $ int2Dyn 4]

  where
  int2Dyn :: Int -> Dynamic
  int2Dyn= toDyn
  
--profits :: TransientIO Int
--profits= do
--      quantity <-   waitEvent "quantity"
--      liftIO $ do
--            putStr "quantity="
--            print (quantity :: Int)
--            getChar
--      price <-   waitEvent "price"
--      liftIO $ do
--            putStr "price="
--            print price
--            getChar
--      liftIO $ do
--         putStr $ "total="
--         print $ quantity * price
--         getChar
--      return $ quantity * price

-- /show



newtype Operation= Operation String deriving Typeable

wprint= liftIO . putStrLn



getInt :: Maybe Int -> TransientIO Int
getInt _= waitEvents  $ getLine' $ const True
getString ::  Maybe String -> TransientIO String
getString _= waitEvents  $ getLine' $ const True

getDouble :: Maybe Double -> TransientIO Double
getDouble _= waitEvents  $ getLine' $ const True


toElem= id

fromStr= id

switchOnOff= on <|> off
  where
  on= do
     option "1" "On"
     wprint "enter total amount of money"
     total <- getInt Nothing
     liftIO $ do
       tryTakeMVar rtotal
       putMVar rtotal total
     atm

  off= do
     option "2" "Off"
     liftIO $ takeMVar ractive
     wprint "ATM stopped"

ractive= unsafePerformIO $ newMVar False

atm= do
   card <- waitCard
   wprint "Enter PIN"
   pin  <- getString Nothing
   validateBank pin card
   setSData card
   clearScreen
   liftIO $ takeMVar ractive
   performTransactions <|> cancel
   liftIO $ putMVar ractive False
   returnCard

performTransactions = do
    withdrawal <|> deposit <|> transfer <|> balanceInquiry
    printReceipt
    return ()

withdrawal= do
    option "1" $ toElem "withdrawall"
    wprint "choose bank account"
    account <- chooseAccount
    wprint "Enter the quantity"
    quantity <- getInt Nothing
    if quantity `rem` 20 /= 0
      then do
        wprint "multiples of $20.00 please"
        stop
      else do
        r <- approbalBank account quantity
        case r of
            False -> do
                wprint "operation denied. sorry"
                wprint "Another transaction?"
                r <- option True  "yes " <|> option False "No"
                if not r then return ()
                                 else performTransactions
            True  ->  giveMoney r

deposit= do
    option "2" "Deposit "
    wprint "choose bank account"
    account <- chooseAccount
    r <- approbalBankDeposit account
    case r of
        False -> do wprint "operation denied. sorry"
                    stop
        True  -> do
            r <- waitDeposit <|> timeout
            case r of
                False -> do wprint "timeout, sorry"; stop
                True  -> return ()

transfer= do
    option "3"  "Transfer "
    wprint "From"
    ac <- chooseAccount
    wprint "amount"
    amount <- getDouble Nothing
    wprint "To"
    ac' <- chooseAccount
    transferAccBank ac ac' amount
    return()

balanceInquiry= do
    option "4" "BalanceInquiry "
    wprint "From"
    ac <- chooseAccount
    r <- getBalanceBank ac
    wprint $ "balance= "++ show r

validateBank pin card = validate' pin card (0 :: Int)
   where
   validate' pin card times= do
    r <- verifyPinBank pin card
    if r then return () else do
     if times ==2
      then do
        wprint ("three tries. card will be retained" :: String)
        stop

      else validate' pin card $ times + 1

rtotal= unsafePerformIO $ newEmptyMVar



type AccountNumber= String
newtype Card= Card [AccountNumber]  deriving Typeable

waitCard = do
  option "1" "enter card"
  return $ Card ["Account1","Account2"]


cancel= do
  option "5" "Cancel"
  return ()

returnCard= wprint "Card returned"

clearScreen=  wprint "clear screen"


printReceipt= do
    Operation str <- getSData <|> error "no operation"
    wprint $ "receipt: Operation:"++ str

chooseAccount= do
    Card accounts <- getSData <|> error "transfer: no card"
    wprint "choose an account"
    mconcat[option ac (fromStr $ ' ':show ac) | ac <- accounts]

approbalBank ac quantity= return True

giveMoney n= wprint $ "Your money : " ++ show n ++ " Thanks"

approbalBankDeposit ac= return True

transferAccBank ac ac' amount= wprint $ "transfer from "++show ac ++ " to "++show ac ++ " done"

getBalanceBank ac= liftIO $ do
    r <- rand
    return $ r * 1000

verifyPinBank _ _= liftIO $ do
    r <- rand
    if r > 0.2 then return True else return False

waitDeposit = do
     n <- liftIO rand
     if n > 0.5 then return True else return False


rand:: IO Double
rand=  randomIO

timeout= return False

