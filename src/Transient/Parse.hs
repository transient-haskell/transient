{-#LANGUAGE ExistentialQuantification, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Transient.Parse where
import Transient.Internals
import Data.String
import Data.Typeable
import Control.Applicative
import Data.Char
import Data.Monoid
import System.IO.Unsafe
import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8             as BS

-- | set a stream of strings to be parsed
setParseStream :: (Typeable str,IsString str) => IO str -> TransIO ()
setParseStream iox= setState $ ParseContext iox ""

-- | set a string to be parsed
setParseString :: (Typeable str,IsString str) => str -> TransIO ()
setParseString x = setState $ ParseContext (error "end of parse string") x 

-- | The parse context contains either the string to be parsed or a computation that gives an stream of
-- strings or both. First, the string is parsed. If it is empty, the stream is pulled for more.
data ParseContext str = IsString str => ParseContext (IO  str) str deriving Typeable


string :: BS.ByteString -> TransIO BS.ByteString
string s=withData $ \str -> do
    let len= BS.length s
        ret@(s',_) = BS.splitAt len str
    if s == s'
      then return ret
      else empty

-- many p= do
--         r <- p 
--         rs <- many p <|> return []
--         return $ r:rs 

manyTill p end  = scan
      where
      scan  = do{ end; return [] }
            <|>
              do{ x <- p; xs <- scan; return (x:xs) }


dropSpaces= parse $ \str ->((),BS.dropWhile isSpace str)

dropChar= parse  $ \r -> ((), BS.tail r)

endline c= c== '\r' || c =='\n'

--tGetLine= tTakeWhile . not . endline

parseString= do
    dropSpaces
    tTakeWhile (not . isSpace)

tTakeWhile :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile cond= parse (BS.span cond)

tTakeWhile' :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile' cond= parse ((\(h,t) -> (h, if BS.null t then t else BS.tail t)) . BS.span cond)

tTake n=  parse ( BS.splitAt n)

tChar= parse $ \s -> (BS.head s,BS.tail s)

parse :: (BS.ByteString -> (b, BS.ByteString)) -> TransIO b
parse split= withData $ \str ->
     if str== mempty   then empty
     else  return $ split str


-- | bring the data of a parse context as a lazy byteString to a parser
-- and actualize the parse context with the result
withData :: (BS.ByteString -> TransIO (a,BS.ByteString)) -> TransIO a
withData parser= Transient $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
   let loop = unsafeInterleaveIO $ do
           r <-  readMore
           (r <>) `liftM` loop
   str <- liftIO $ (s <> ) `liftM` loop
   mr <- runTrans $ parser str
   case mr of
    Nothing -> return Nothing
    Just (v,str') -> do
      setData $ ParseContext readMore str'
      return $ Just v

-- | bring the data of the parse context as a lazy byteString
giveData= noTrans $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
                                  :: StateIO (ParseContext BS.ByteString)  -- change to strict BS

   let loop = unsafeInterleaveIO $ do
           r <-  readMore
           (r <>) `liftM` loop
   liftIO $ (s <> ) `liftM` loop
