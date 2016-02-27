{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,  CPP #-}


module Main where

import Transient.Base
#ifdef ghcjs_HOST_OS
   hiding ( option,runCloud)
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option,runCloud)
#endif


import Transient.Move
import Transient.Logged
import Transient.Stream.Resource
import Control.Monad.IO.Class
import Control.Applicative

import System.Environment
import Control.Monad
import Control.Concurrent

import Transient.Indeterminism
import Data.Monoid
import Data.Typeable


import Data.IORef
import Control.Monad.State
import Control.Concurrent

import System.IO.Unsafe

main3= Transient.Base.keep $ do
    r <- (liftIO $ print "called") **> option "a" "a"
    liftIO $ print "next"

main2 = runCloud $ do
    let nodes= map createLocalNode [2000,2001]
    cexec $ addNodes nodes

    (foldl (<|>) empty $ map listen  nodes) <|> return ()

    local $ option "start" "start"
    wormhole (nodes !! 1) $ do
        r <- local sequ
        teleport
        cexec $ liftIO $ print r
    where
    createLocalNode n= createNode "localhost"  n



main = runCloud $  do
    args <- cexec $ liftIO getArgs

    let localPort =  2020 -- read (args !! 0)
        serverPort  = 2020 -- read (args !! 1)

--
        serverNode  = createNode "localhost" serverPort
        mynode    = if isBrowserInstance
                       then  WebNode  (unsafePerformIO emptyPool)
                       else createNode "localhost" localPort

--    connect mynode serverNode

    listen1 mynode serverNode

      --  <|>  (local addPrefix >> widget serverNode)


    render $   widget  <|>  widget


widget =  do
         local addPrefix
         op <-  local  $ option "fire" "fire"   <|>  option "cancel" "cancel"

         teleport
         context <- get
         r <- local $ case op of
                   "fire" ->    stopContext context >> sequ
                   "cancel" ->  stopContext context >> empty
         teleport
         local $ rawHtml $ h1 r

stopContext cont= liftIO $ killChildren cont

testw=  do
     addPrefix
     op <- option "fire" "fire" --  <|>  option "cancel" "cancel"
     context <- get
     r <- case op of
           "fire"   -> stopCounter context >> sequ
           "cancel" -> stopCounter context >> empty
     rawHtml $ h1 r

stopCounter  context= liftIO (killChildren context)

sequ= do

     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n



--    local $ render $ do
--      r <- render (inputSubmit "1111" `fire` OnClick  !!> "FIRST")
--      render (inputSubmit "2222" `fire` OnClick  !!> "SECON")
--      render $ wraw $ toElem " returns: " <> b (show r)
--       buttons <|> linksample
--      render $ do



{-
    linksample= do
     r <-   wlink "Hi!" (toElem "This link say Hi!")`fire` OnClick
     wraw( b (" returns "++ r))

    buttons= p "Different input elements:" ++> checkButton
                                       **> br ++> br
                                       ++> radio
                                       **> br ++> br
                                       ++> select
                                       <++ br

    checkButton=do
       rs <- getCheckBoxes(
                       ((setCheckBox False "Red"    <++ b "red"  )  `fire` OnClick)
                    <> ((setCheckBox False "Green"  <++ b "green")  `fire` OnClick)
                    <> ((setCheckBox False "blue"   <++ b "blue" )  `fire` OnClick))
       wraw $ toElem " returns: " <> b (show rs)

    radio= do
       r <- getRadio [toElem v ++> setRadioActive v  | v <- ["red","green","blue"]]

       wraw $ toElem " returns: " <> b ( show r )

    select= do
       r <- getSelect (   setOption "red"   (toElem "red")
                      <|> setOption "green" (toElem "green")
                      <|> setOption "blue"  (toElem "blue"))
              `fire` OnClick

       wraw $ toElem " returns: " <> b ( show r )

-}

{-
    liftIO $ threadDelay 2000000
    liftIO $ print "------------------"
--    x <- local $ return "hello"
    r <- wormhole serverNode $ do
          teleport
          r <- local $ return "world"
          teleport
          return r
    liftIO $ print r
--    listen mynode <|> return ()
-}
--    cexec $ (h2 <<< (wraw $ p "hello")) <++ b "world"
{-
    r<- local $  (inputSubmit "start"  `fire` OnClick >> return "ojjljkjj" )
             <|>  (inputSubmit "cancel"  `fire` OnClick !!> "klklklllk2")
    r' <- local $ inputSubmit "end2" `fire` OnClick
    cexec $ wraw $ h1 r -- (r,r')
-}
{-
    wormhole serverNode $ do
        r<- local $ inputSubmit "start"  `fire` OnClick
                <|> inputSubmit "cancel" `fire` OnClick
            --option "start" "start" <|> option "cancel" "cancel"
        teleport
        r' <- local $ case r of
            "start"  -> waitEvents $ threadDelay 1000000 >> return  "hello"
            "cancel" -> killSibling >> empty
        teleport
        wprint r'
-}

{-
streamToFrom node to from= wormhole node $ do
     r <- to
     teleport
     r' <- from r
     teleport
     return r'
-}
{-
    wormhole node1 $ do
       logged $ liftIO $ putStrLn "input"
       n <- logged $ input (const True)
       teleport
       logged $ liftIO $ print (n :: Int)

       n <- logged $ return $ n + 1
       teleport
       logged $ liftIO $ print n
       n <- logged $ return $ n + 1
       teleport
       logged $ liftIO $ print n

-}
{-
    wh <- wormhole node1
    x <- logged $ choose [1,2 :: Int]
    logged $ putStr "sending ">> print x
    teleport wh
    logged $ liftIO $ print x
    x'<- logged $ return $ x+1
    teleport wh
    liftIO $ print x'
-}

{-
    r <- wormhole node1   $ do
                teleport     !!> "TELEPORT 1"
--                r <- step (return "hello")

                r <- wormhole node2 $ do
                       teleport     !!> "TELEPORT 11"
                       logged $ liftIO $ print "WORLD"
                       r <- logged $ return  "world"

                       teleport     !!> "TELEPORT 12"
                       return r
                step $ liftIO $ print "RETURNINGGGGGGGGGGG"
                teleport  !!> "TELEPORT 2"
                return r

    logged $ liftIO $ print r
-}

{-
    r <- callTo node1 $ do
--           option "cont" "cont"
           local $ liftIO $ print "->HELLO"
           r <- callTo node2 $do
                   local $ liftIO $ print "->WORLD" >> return "world"


           return ("hello", r)

    liftIO $ print r

-}






{-
        r <- logged $ return "HALLO"
        wh@(WormHole conn _) <- wormhole serverNode
        liftIO $ print "AFTER WORMHOLE"

        liftIO $  print "______________________________________"

        r <- logged $ liftIO $ print "MESSAGE 1111 " >> return "hello"



        teleport wh
--        logged option "fire2"  "after teleport 1"
        logged $ liftIO $ print "MESSAGE 222222"


        liftIO $ print r  !!> "PRINTR"
        r <- logged $ return "world"
--        liftIO $ print "HERE SHOULD COME THE SECOND TELEPORT"
        teleport wh
--        logged $ option "fire3" "after teleport 2"
        r <- logged $ liftIO $ print "MESSAGE 33333" >> return "world2"
        liftIO $ print r
        teleport wh
        liftIO $ print "message 44444"
        liftIO $ print r
-}

--        when r $ do
----             liftIO $ mclose conn
--             killChilds

--        counter <- liftIO $ newIORef (0 :: Int)
--        r <- parallel $ do
--                   n <- atomicModifyIORef counter $ \r -> (r +1,r)
--                   threadDelay 10000000
--                   return (SMore n)
--        teleport wh
--        return r


--        liftIO $ print r



--    local $  render $ ( getInt Nothing)   <|>  (( p "jkjkjk" `pass1` OnClick)  >> return 1)


