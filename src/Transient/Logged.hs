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

Log(..), toLazyByteString, byteString, lazyByteString
) where

import Data.Typeable
import Unsafe.Coerce
import Transient.Internals

import Transient.Indeterminism(choose)
--import Transient.Internals -- (onNothing,reads1,IDynamic(..),Log(..),LogElem(..),execMode(..),StateIO)
import Transient.Parse
import Control.Applicative
import Control.Monad.State
import System.Directory
import Control.Exception
--import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import qualified Data.Map as M
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

exec=  byteString "e/"
wait=  byteString "w/"

class (Show a, Read a,Typeable a) => Loggable a where
    serialize :: a -> Builder
    serialize = byteString .   BSS.pack . show

    deserializePure :: BS.ByteString -> Maybe(a, BS.ByteString)
    deserializePure s = r
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

instance Loggable ()

instance Loggable Bool where 
  serialize b= if b then "t" else "f"
  deserialize = withGetParseString $ \s -> 
     if (BS.head $ BS.tail s) /= '/'   
        then empty 
        else
            let h= BS.head s
                tail=  BS.tail s
            in if h== 't' then return (True,tail)  else if h== 'f' then return (False, tail) else empty 

instance Loggable Int

instance (Typeable a, Loggable a) => Loggable[a]  
--    serialize x= if typeOf x= typeOf (undefined :: String) then BS.pack x else BS.pack $ show x
--    deserialize= let [(s,r)]= 




instance Loggable Char
instance Loggable Float
instance Loggable Double
instance Loggable a => Loggable (Maybe a)
instance (Loggable a,Loggable b) => Loggable (a,b)
instance (Loggable a,Loggable b, Loggable c) => Loggable (a,b,c)
instance (Loggable a,Loggable b, Loggable c,Loggable d) => Loggable (a,b,c,d)

instance (Loggable a, Loggable b) => Loggable (Either a b)
-- #ifdef ghcjs_HOST_OS


-- intDec i= Builder $ \s -> pack (show i) <> s
-- int64Dec i=  Builder $ \s -> pack (show i) <> s

-- #endif
instance (Loggable k, Ord k, Loggable a) => Loggable (M.Map k a)  where
  serialize v= intDec (M.size v) <> M.foldlWithKey' (\s k x ->  s <> "/" <> serialize k <> "/" <> serialize x ) mempty v
  deserialize= do
      len <- int
      list <- replicateM len $
                 (,) <$> (tChar '/' *> deserialize)
                     <*> (tChar '/' *> deserialize)
      return $ M.fromList list

#ifndef ghcjs_HOST_OS
instance Loggable BS.ByteString where
        serialize str =  lazyByteString str
        deserialize= tTakeWhile (/= '/')
#endif

#ifndef ghcjs_HOST_OS
instance Loggable BSS.ByteString where
        serialize str =  byteString str
        deserialize = tTakeWhile (/= '/') >>= return . BS.toStrict
#endif
instance Loggable SomeException



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
         setParseString log
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

getLog :: TransMonad m =>  m Log
getLog= getData `onNothing` return emptyLog

emptyLog= Log False mempty mempty 0 0


-- | Run the computation, write its result in a log in the state
-- and return the result. If the log already contains the result of this
-- computation ('restore'd from previous saved state) then that result is used
-- instead of running the computation again.
--
-- 'logged' can be used for computations inside a nother 'logged' computation. Once
-- the parent computation is finished its internal (subcomputation) logs are
-- discarded.
--


logged :: Loggable a => TransIO a -> TransIO a
logged mx =   do
        log <- getLog

        let full= fulLog log
        rest <- giveParseString

        if recover log                  -- !> ("recover",recover log)
           then
                  if not $ BS.null rest -- rest <= 1)
                    then recoverIt log   -- !> "RECOVER"  -- exec1 log <|> wait1 log <|> value log
                    else do
                      setData log{buildLog=mempty}
                      notRecover full log  !>  "NOTRECOVER"

           else notRecover full log
    where
    notRecover full log= do

        let rs  = buildLog log 
        setData $ Log False (rs <> exec) (full <> exec) (lengthFull log +1)  (hashClosure log + 1000)     -- !> ("setLog False", Exec:rs)

        r <-  mx <** do setData $ Log False ( rs <>  wait) (full <> wait) (lengthFull log +1) (hashClosure log + 100000)
                            -- when   p1 <|> p2, to avoid the re-execution of p1 at the
                            -- recovery when p1 is asynchronous or return empty

        log' <- getLog     


        let len= lengthFull log'
            add= full <> serialize r <> byteString "/"   -- Var (toIDyn r):  full
            recoverAfter= recover log'
            lognew= buildLog log'

        rest <- giveParseString
        if recoverAfter && not (BS.null rest) 
          then  do
            modify $ \s -> s{execMode= Parallel}   
            setData $ log'{recover= True, fulLog=  lognew <> add,  lengthFull= lengthFull log+ len,hashClosure= hashClosure log + 10000000}
                                                    --  !> ("recover",  toLazyByteString $ lognew <> add)

          else

            setData $ Log{recover= False, buildLog=rs <> serialize r  <> byteString "/", fulLog= add,lengthFull= len+1, hashClosure=hashClosure log +10000000}
               


        return  r

    recoverIt log= do
        s <- giveParseString
        case BS.splitAt 2 s of
          ("e/",r) -> do
            setData $ log{ 
            lengthFull= lengthFull log +1, hashClosure= hashClosure log + 1000}
            setParseString r                     --   !> "Exec"
            mx

          ("w/",r) -> do
            setData $ log{ 
            lengthFull= lengthFull log +1, hashClosure= hashClosure log + 100000}
            setParseString r
            modify $ \s -> s{execMode= Parallel}  --setData Parallel
            empty                                --   !> "Wait"

          _ -> value log

    value log= r
      where
      typeOfr :: TransIO a -> a
      typeOfr _= undefined
      r= do
            -- return() !> "logged value"
            x <- deserialize <|> do
                   psr <- giveParseString
                   error  (show("error parsing",psr,"to",typeOf $ typeOfr r))
                  
            tChar '/'

            setData $ log{recover= True -- , = rs'
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
       (Transient.Internals.try $ tChar '/'  >> return ())<|> return () --maybe there is a '/' to drop
       --(Transient.Internals.try $ tTakeWhile (/= '/') >>= liftIO . print >> empty) <|> return ()
       if      t == typeOf (undefined :: String)     then return . unsafeCoerce . BS.unpack =<< tTakeWhile' (/= '/')
       else if t == typeOf (undefined :: BS.ByteString) then return . unsafeCoerce =<< tTakeWhile' (/= '/')
       else if t == typeOf (undefined :: BSS.ByteString)  then return . unsafeCoerce . BS.toStrict =<< tTakeWhile' (/= '/')
       else deserialize  -- <* tChar '/'


       where
       type1  :: TransIO x ->  x
       type1 = undefined


