{-#LANGUAGE ExistentialQuantification, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
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
-- import Control.Exception (throw,IOException)



import qualified Data.ByteString.Lazy.Char8             as BS

-- | set a stream of strings to be parsed
setParseStream ::  IO (StreamData BS.ByteString) -> TransIO ()
setParseStream iox= setState $ ParseContext iox ""

-- | set a string to be parsed
setParseString :: BS.ByteString -> TransIO ()
setParseString x = setState $ ParseContext (return SDone) x 

-- | The parse context contains either the string to be parsed or a computation that gives an stream of
-- strings or both. First, the string is parsed. If it is empty, the stream is pulled for more.
data ParseContext str = IsString str => ParseContext (IO  (StreamData str)) str deriving Typeable


-- | succeed if read the string given as parameter
string :: BS.ByteString -> TransIO BS.ByteString
string s=withData $ \str -> do
    let len= BS.length s
        ret@(s',_) = BS.splitAt len str
    if s == s' 
      then return ret
      else empty

-- | fast search for a token
dropUntilToken token= withData $ \str -> 
    if BS.null str then empty else return $ drop2 str 
  where 
  drop2 str=
   if BS.null str then ((),str) else 
      if token `BS.isPrefixOf` str 
          then  ((),BS.drop (BS.length token) str)
          else  drop2 $ BS.tail str 

-- | read an Integer
integer :: TransIO Integer
integer= do
    s <- tTakeWhile isNumber
    if BS.null  s  then empty else return $ stoi 0 s
  :: TransIO Integer

   where
   stoi :: Integer -> BS.ByteString -> Integer
   stoi x s| BS.null s = x
           | otherwise=  stoi (x *10 + fromIntegral(ord (BS.head s) - ord '0')) (BS.tail s)



-- | read an Int
int :: TransIO Int
int= do 
    s <- tTakeWhile isNumber
    if BS.null s then empty else return $ stoi 0 s

    where
    stoi :: Int -> BS.ByteString -> Int
    stoi x s| BS.null s = x
            | otherwise=  stoi (x *10 + (ord (BS.head s) - ord '0')) (BS.tail s)


-- | read many results with a parser (at least one) until a `end` parser succeed.
manyTill p end  = (:) <$> p <*> scan
      where
      scan  = do{ end; return [] }
            <|>
              do{ x <- p; xs <- scan; return (x:xs) }


dropSpaces= parse $ \str ->((),BS.dropWhile isSpace str)



parseString= do
    dropSpaces
    tTakeWhile (not . isSpace)

tTakeWhile :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile cond= parse (BS.span cond)

tTakeWhile' :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile' cond= parse ((\(h,t) -> (h, if BS.null t then t else BS.tail t)) . BS.span cond)

-- | take n characters 
tTake n=  parse ( BS.splitAt n)

-- | read a char
anyChar= parse $ \s -> (BS.head s,BS.tail s)

-- | 
tChar c= anyChar >>= \x -> if x== c then return c else empty

parse :: (BS.ByteString -> (b, BS.ByteString)) -> TransIO b
parse split= withData $ \s ->
     if s == mempty then  empty else return $ split s 


-- | bring the data of a parse context as a lazy byteString to a parser
-- and actualize the parse context with the result
withData :: (BS.ByteString -> TransIO (a,BS.ByteString)) -> TransIO a
withData parser= (Transient $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
   
   let loop = unsafeInterleaveIO $ do
           mr <-  readMore
           case mr of 
             SMore r ->  (r <>) `liftM` loop
             SLast r ->  (r <>) `liftM` loop
             SDone -> return mempty
   str <- liftIO $ (s <> ) `liftM` loop
   mr <- runTrans $ parser str
   case mr of
    Nothing -> return Nothing     -- !> "NOTHING"
    Just (v,str') -> do
      setData $ ParseContext readMore str'
      return $ Just v)

-- | bring the data of the parse context as a lazy byteString
giveData= (noTrans $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
                                  :: StateIO (ParseContext BS.ByteString)  -- change to strict BS

   let loop = unsafeInterleaveIO $ do
           mr <-  readMore
           case mr of 
            SMore r ->  (r <>) `liftM` loop
            SLast r ->  (r <>) `liftM` loop
            SDone -> return mempty
   liftIO $ (s <> ) `liftM` loop)

-- | True if the stream has finished
isDone :: TransIO Bool
isDone=  noTrans $ do 
    ParseContext readMore s <- getData `onNothing` error "parser: no context"
       :: StateIO (ParseContext BS.ByteString)  -- change to strict BS
    if not $ BS.null s then return False else do
      mr <- liftIO readMore 
      case mr of 
        SMore r -> do setData $ ParseContext readMore r ; return False
        SLast r -> do setData $ ParseContext readMore r ; return False
        SDone -> return True

-- tryUntilDone parser= parser `catcht` \(e :: IOException) -> empty