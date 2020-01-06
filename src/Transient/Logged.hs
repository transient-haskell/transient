 {-#Language OverloadedStrings, FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Transient.Logged
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | The 'logged' primitive is used to save the results of the subcomputations
-- of a transient computation (including all its threads) in a log buffer. At
-- any point, a 'suspend' or 'checkpoint' can be used to save the accumulated
-- log on a persistent storage. A 'restore' reads the saved logs and resumes
-- the computation from the saved checkpoint. On resumption, the saved results
-- are used for the computations which have already been performed. The log
-- contains purely application level state, and is therefore independent of the
-- underlying machine architecture. The saved logs can be sent across the wire
-- to another machine and the computation can then be resumed on that machine.
-- We can also save the log to gather diagnostic information.
--
-- The following example illustrates the APIs. In its first run 'suspend' saves
-- the state in a directory named @logs@ and exits, in the second run it
-- resumes from that point and then stops at the 'checkpoint', in the third run
-- it resumes from the checkpoint and then finishes.
--
-- @
-- main= keep $ restore  $ do
--      r <- logged $ choose [1..10 :: Int]
--      logged $ liftIO $ print (\"A",r)
--      suspend ()
--      logged $ liftIO $ print (\"B",r)
--      checkpoint
--      liftIO $ print (\"C",r)
-- @
-----------------------------------------------------------------------------
{-# LANGUAGE  CPP, ExistentialQuantification, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Transient.Logged(
Loggable(..), logged, received, param, getLog, exec,wait, emptyLog,

#ifndef ghcjs_HOST_OS
 suspend, checkpoint, rerun, restore,
#endif

Log(..),Builder(..), toLazyByteString, byteString, lazyByteString
) where

import Data.Typeable
import Unsafe.Coerce
import Transient.Internals

import Transient.Indeterminism(choose)
import Transient.Internals -- (onNothing,reads1,IDynamic(..),Log(..),LogElem(..),execMode(..),StateIO)
import Transient.Parse
import Control.Applicative
import Control.Monad.State
import System.Directory
import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.Maybe (fromJust)
import System.IO.Unsafe

-- #ifndef ghcjs_HOST_OS
import Data.ByteString.Builder
import Data.Monoid
import System.Random
-- #else
--import Data.JSString hiding (empty)
-- #endif



-- #ifndef ghcjs_HOST_OS
-- pack= BSS.pack

-- #else
{-
newtype Builder= Builder(JSString -> JSString)
instance Monoid Builder where
   mappend (Builder fx) (Builder fy)= Builder $ \next -> fx (fy next)
   mempty= Builder id

instance Semigroup Builder where
    (<>)= mappend

byteString :: JSString -> Builder
byteString ss= Builder $ \s -> ss <> s
lazyByteString = byteString


toLazyByteString :: Builder -> JSString
toLazyByteString (Builder b)=  b  mempty
-}
-- #endif

exec=  byteString "Exec/"
wait=  byteString "Wait/"

class (Show a, Read a,Typeable a) => Loggable a where
    serialize :: a -> Builder
    serialize = byteString .   BSS.pack . show

    deserializePure :: BS.ByteString -> Maybe(a, BS.ByteString)
    deserializePure s= r
      where
      r= case reads $ BS.unpack s   of -- `traceShow` ("deserialize",typeOf $ typeOf1 r,s) of
           []       -> Nothing  !> "Nothing"
           (r,t): _ -> return (r, BS.pack t)

      typeOf1 :: Maybe(a, BS.ByteString) -> a
      typeOf1= undefined

    deserialize ::  TransIO a
    deserialize = x
       where
       x=  withGetParseString $ \s -> case deserializePure s of
                    Nothing ->   empty
                    Just x -> return x




data Log    = Log{ recover :: Bool, buildLog :: Builder, fulLog :: Builder, lengthFull:: Int, hashClosure :: Int}




#ifndef ghcjs_HOST_OS



-- | Reads the saved logs from the @logs@ subdirectory of the current
-- directory, restores the state of the computation from the logs, and runs the
-- computation.  The log files are maintained.
-- It could be used for the initial configuration of a program.
rerun :: String -> TransIO a -> TransIO a
rerun path proc = do
     liftIO $ do
         r <- doesDirectoryExist path
         when (not r) $ createDirectory  path
         setCurrentDirectory path
     restore' proc False



logs= "logs/"

-- | Reads the saved logs from the @logs@ subdirectory of the current
-- directory, restores the state of the computation from the logs, and runs the
-- computation.  The log files are removed after the state has been restored.
--
restore :: TransIO a -> TransIO a
restore   proc= restore' proc True

restore' proc delete= do
     liftIO $ createDirectory logs  `catch` (\(e :: SomeException) -> return ())
     list <- liftIO $ getDirectoryContents logs
                 `catch` (\(e::SomeException) -> return [])
     if null list || length list== 2 then proc else do

         let list'= filter ((/=) '.' . head) list
         file <- choose  list'

         log <- liftIO $ BS.readFile (logs++file)

         let logb= lazyByteString  log
         setData Log{recover= True,buildLog= logb,fulLog= logb,lengthFull= 0, hashClosure= 0}
         when delete $ liftIO $ remove $ logs ++ file
         proc
     where
     -- read'= fst . head . reads1

     remove f=  removeFile f `catch` (\(e::SomeException) -> remove f)



-- | Saves the logged state of the current computation that has been
-- accumulated using 'logged', and then 'exit's using the passed parameter as
-- the exit code. Note that all the computations before a 'suspend' must be
-- 'logged' to have a consistent log state. The logs are saved in the @logs@
-- subdirectory of the current directory. Each thread's log is saved in a
-- separate file.
--
suspend :: Typeable a =>  a -> TransIO a
suspend  x= do
   log <- getLog
   if (recover log) then return x else do
        logAll  $ fulLog log
        exit x


-- | Saves the accumulated logs of the current computation, like 'suspend', but
-- does not exit.
checkpoint :: TransIO ()
checkpoint = do
   log <- getLog
   if (recover log) then return () else logAll  $ fulLog log


logAll log= liftIO $do
        newlogfile <- (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
        logsExist <- doesDirectoryExist logs
        when (not logsExist) $ createDirectory logs
        BS.writeFile newlogfile $ toLazyByteString log
      -- :: TransIO ()
#else
rerun :: TransIO a -> TransIO a
rerun = const empty

suspend :: TransIO ()
suspend= empty

checkpoint :: TransIO ()
checkpoint= empty

restore :: TransIO a -> TransIO a
restore= const empty

#endif




-- | Run the computation, write its result in a log in the parent computation
-- and return the result. If the log already contains the result of this
-- computation ('restore'd from previous saved state) then that result is used
-- instead of running the computation again.
--
-- 'logged' can be used for computations inside a 'logged' computation. Once
-- the parent computation is finished its internal (subcomputation) logs are
-- discarded.
--
getLog :: TransMonad m =>  m Log
getLog= getData `onNothing` return emptyLog

emptyLog= Log False mempty mempty 0 0

logged :: Loggable a => TransIO a -> TransIO a
logged mx =   do
        log <- getLog

        -- setParseString $ toLazyByteString  $ buildLog log

        let full= fulLog log
        rest <- giveParseString
        -- let rest=  toLazyByteString $ buildLog  log

        if recover log
           then
                  if not $ BS.null rest -- rest <= 1)
                    then recoverIt log -- exec1 log <|> wait1 log <|> value log
                    else do
                      setData log{buildLog=mempty}
                      notRecover full log

           else notRecover full log
    where
    notRecover full log= do

        let rs  = buildLog log -- giveParseString >>= return . lazyByteString
        -- return () !> ("BUILDLOG", toLazyByteString rs)
        setData $ Log False (rs <> exec) (full <> exec) (lengthFull log +1)  (hashClosure log + 1000)     -- !> ("setLog False", Exec:rs)

        r <-  mx <** do setData $ Log False ( rs <>  wait) (full <> wait) (lengthFull log +1) (hashClosure log + 100000)
                            -- when   p1 <|> p2, to avoid the re-execution of p1 at the
                            -- recovery when p1 is asynchronous or return empty

        log' <- getLog     -- Log recoverAfter lognew len _ <- getLog


        let len= lengthFull log'
            add= full <> serialize r <> byteString "/"   -- Var (toIDyn r):  full
            recoverAfter= recover log'
            lognew= buildLog log'

        rest <- giveParseString
        if recoverAfter && not (BS.null rest)        --  !> ("recoverAfter", recoverAfter)
          then  do
            modify $ \s -> s{execMode= Parallel}  --setData Parallel
            setData $ log'{recover= True, fulLog=  lognew <> add,  lengthFull= lengthFull log+ len,hashClosure= hashClosure log + 10000000}
                                                      !> ("recover",  toLazyByteString $ lognew <> add)

          else

            setData $ Log{recover= False, buildLog=rs <> serialize r  <> byteString "/", fulLog= add,lengthFull= len+1, hashClosure=hashClosure log +10000000}
                -- Log False (Var (toIDyn r):rs) add (hashClosure +10000000))
                         --  !> ("restore",  toLazyByteString $serialize r <> rs)


        return  r

    recoverIt log= do
        s <- giveParseString
        case BS.splitAt 5 s of
          ("Exec/",r) -> do
            setData $ log{ -- recover= True,  --  buildLog=  rs',
            lengthFull= lengthFull log +1, hashClosure= hashClosure log + 1000}
            setParseString r                     --   !> "Exec"
            mx

          ("Wait/",r) -> do
            setData $ log{ -- recover= True, --  buildLog=  rs',
            lengthFull= lengthFull log +1, hashClosure= hashClosure log + 100000}
            setParseString r
            modify $ \s -> s{execMode= Parallel}  --setData Parallel
            empty                                --   !> "Wait"

          _ -> value log

    value log= r
      where
      typeOfr :: TransIO a -> a
      typeOfr x= undefined
      r= do
            -- return() !> "logged value"
            x <- deserialize  <|> do
                   psr <- giveParseString
                   error  (show("error parsing",psr,"to",typeOf $ typeOfr r))
            -- rs'  <- giveParseString >>= return . lazyByteString
            tChar '/'

            setData $ log{recover= True -- , buildLog= rs'
                         ,lengthFull= lengthFull log +1,hashClosure= hashClosure log + 10000000}
            return x





-------- parsing the log for API's

received :: (Loggable a, Eq a) => a -> TransIO ()
received n= Transient.Internals.try $ do
   r <- param
   if r == n then  return () else empty

param :: (Loggable a, Typeable a) => TransIO a
param = r where
  r=  do
       let t = typeOf $ type1 r
       (Transient.Internals.try $ tTakeWhile (/= '/') >>= liftIO . print >> empty) <|> return ()
       if      t == typeOf (undefined :: String)     then return . unsafeCoerce . BS.unpack =<< tTakeWhile' (/= '/')
       else if t == typeOf (undefined :: BS.ByteString) then return . unsafeCoerce =<< tTakeWhile' (/= '/')
       else if t == typeOf (undefined :: BSS.ByteString)  then return . unsafeCoerce . BS.toStrict =<< tTakeWhile' (/= '/')
       else deserialize <* tChar '/'


       where
       type1  :: TransIO x ->  x
       type1 = undefined


