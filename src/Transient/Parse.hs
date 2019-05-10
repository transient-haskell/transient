{-#LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Transient.Parse where
import Transient.Internals
import Transient.Indeterminism
import Data.String
import Data.Typeable
import Control.Applicative
import Data.Char
import Data.Monoid

import System.IO.Unsafe
import Control.Monad
import Control.Monad.State
-- import Control.Exception (throw,IOException)
import Control.Concurrent.MVar


import qualified Data.ByteString.Lazy.Char8  as BS

-- | set a stream of strings to be parsed
setParseStream ::  IO (StreamData BS.ByteString) -> TransIO ()

setParseStream iox= do delData NoRemote; setState $ ParseContext iox ""

-- | set a string to be parsed
setParseString :: BS.ByteString -> TransIO ()
setParseString x = do delData NoRemote; setState $ ParseContext (return SDone) x 

withParseString :: BS.ByteString -> TransIO a -> TransIO a
withParseString x parse= do
     p@(ParseContext c str) <- getState <|> return(ParseContext (return SDone) mempty)
     setParseString x
     r <- parse
     setState (ParseContext c (str :: BS.ByteString))
     return r

-- | The parse context contains either the string to be parsed or a computation that gives an stream of
-- strings or both. First, the string is parsed. If it is empty, the stream is pulled for more.
data ParseContext str = IsString str => ParseContext (IO  (StreamData str)) str deriving Typeable


-- | succeed if read the string given as parameter
string :: BS.ByteString -> TransIO BS.ByteString
string s= withData $ \str -> do
    let len= BS.length s
        ret@(s',_) = BS.splitAt len str
    if s == s' -- !> ("parse string looked, found",s,s')
      then return ret
      else empty -- !> "STRING EMPTY"

-- | fast search for a token
tDropUntilToken token= withData $ \str -> 
    if BS.null str then empty else  drop2 str 
  where 
  drop2 str=
    if token `BS.isPrefixOf` str  !> (BS.take 2 str)
          then  return ((),BS.drop (BS.length token) str)
          else if not $ BS.null str then drop2 $ BS.tail str else empty

tTakeUntilToken :: BS.ByteString -> TransIO BS.ByteString
tTakeUntilToken token= withData $ \str -> takeit mempty str
  where 
  takeit :: BS.ByteString -> BS.ByteString -> TransIO ( BS.ByteString, BS.ByteString)
  takeit res str= 
   if BS.null str then return (res,str) else 
      if token `BS.isPrefixOf` str 
          then  return (res !> ("tTakeUntilString",res),BS.drop (BS.length token) str)
          else  if not $ BS.null str then takeit ( BS.snoc res (BS.head str)) $ BS.tail str else empty
    
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
    s <- tTakeWhile' isNumber
    if BS.null s then empty else return $ stoi 0 s

    where
    stoi :: Int -> BS.ByteString -> Int
    stoi x s| BS.null s = x
            | otherwise=  stoi (x *10 + (ord (BS.head s) - ord '0')) (BS.tail s)


-- | read many results with a parser (at least one) until a `end` parser succeed.

 

manyTill :: TransIO a -> TransIO b -> TransIO [a]
manyTill= chainManyTill (:)

chainManyTill op p end=  op <$> p <*> scan
      where
      scan  = do{ end; return mempty }
            <|>
              do{ x <- p; xs <- scan; return (x `op` xs) }

between open close p
                    = do{ open; x <- p; close; return x }

symbol = string 
         
parens p        = between (symbol "(") (symbol ")") p  !> "parens "
braces p        = between (symbol "{") (symbol "}") p  !> "braces "
angles p        = between (symbol "<") (symbol ">") p  !> "angles "
brackets p      = between (symbol "[") (symbol "]") p  !> "brackets "

semi            = symbol ";"  !> "semi"
comma           = symbol ","  !> "comma"
dot             = symbol "."  !> "dot"
colon           = symbol ":"  !> "colon"



sepBy p sep         = sepBy1 p sep <|> return []


sepBy1 = chainSepBy1 (:) 


chainSepBy chain p sep= chainSepBy1 chain p sep <|> return mempty

-- take a byteString of elements separated by a separator and  apply the desired operator to the parsed results
chainSepBy1
  :: (Monad m, Monoid b, Alternative m) =>
     (a -> b -> b) -> m a -> m x -> m b
chainSepBy1 chain p sep= do{ x <- p
                        ; xs <- chainMany chain (sep >> p)
                        ; return (x `chain` xs)
                        }
                        !> "chainSepBy "
       
chainMany chain v= (chain <$> v <*> chainMany chain v) <|> return mempty
       
commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi

dropSpaces= withData $ \str -> return( (),BS.dropWhile isSpace str)




dropTillEndOfLine= withData $ \str -> return ((),BS.dropWhile ( /= '\n') str) !> "dropTillEndOfLine"

--manyTill anyChar (tChar '\n' <|> (isDonep >> return ' ') )

parseString= do
    dropSpaces 
    tTakeWhile (not . isSpace)

-- | take characters while they meet the condition
tTakeWhile :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile cond= -- parse (BS.span cond)
    withData $ \s -> let (h,t)= BS.span cond s in if BS.null h then empty else return (h,t) !> ("tTakeWhile",h)
   
   
-- | take characters while they meet the condition and drop the next character
tTakeWhile' :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile' cond= withData $ \s -> do
   let (h,t)= BS.span cond s
   return () !> ("takewhile'",h,t)
   if BS.null h then  empty else return (h, if BS.null t then t else BS.tail t) 

 
just1 f x= let (h,t)= f x in (Just h,t)

-- | take n characters 
tTake n= withData $ \s ->  return $ BS.splitAt n s -- !> ("tTake",n,BS.take n s)

-- | drop n characters
tDrop n= withData $ \s ->  return $ ((),BS.drop n s)

-- | read a char
anyChar= withData $ \s -> if BS.null s then empty else return (BS.head s,BS.tail s) -- !> ("anyChar",s)

-- | verify that the next character is the one expected
tChar c= withData $ \s -> if BS.null s || BS.head s /= c then empty else return (BS.head s,BS.tail s)  !> ("tChar", BS.head s) 
   --  anyChar >>= \x -> if x == c then return c else empty !> ("tChar",x)




-- | bring the lazy byteString state to a parser
-- and actualize the byteString state with the result
-- The tuple that the parser should return should be :  (what it returns, what should remain to be parsed)
withData :: (BS.ByteString -> TransIO (a,BS.ByteString)) -> TransIO a
withData parser= Transient $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
   
   let loop = unsafeInterleaveIO $ do
           mr <-  readMore 

           return () !> ("readMore",mr)
           case mr of 
             SMore r ->  return r  <> loop 
             SLast r ->  return r
             SDone -> return mempty  -- !> "withData SDONE" 
   str <- liftIO $ return s <> loop
   --if str == mempty then return Nothing else do
   mr <- runTrans $ parser str
   case mr of
            Nothing -> return Nothing     -- !> "NOTHING"
            Just (v,str') -> do
                  setData $ ParseContext readMore str'
                  return $ Just v



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
    return () !> "isDone"
    ParseContext readMore s <- getData `onNothing` error "parser: no context"
       :: StateIO (ParseContext BS.ByteString)  -- change to strict BS
    if not $ BS.null s then return False else do
      mr <- liftIO readMore 
      case mr of 
        SMore r -> do setData $ ParseContext readMore r ; return False
        SLast r -> do setData $ ParseContext readMore r ; return False
        SDone -> return True


        
        
        
        
-- infixl 0 |-

-- | Chain two parsers. The motivation is to parse a chunked HTTP response which contains
-- JSON messages.
--
-- If the REST response is infinite and contains JSON messages, I have to chain the 
-- dechunk parser with the JSON decoder of aeson, to produce a stream of aeson messages. 
-- Since the boundaries of chunks and JSON messages do not match, it is not possible to add a 
-- `decode` to the monadic pipeline. Since the stream is potentially infinite and/or the
-- messages may arrive at any time, I can not wait until all the input finish before decoding 
-- the messages.
--
-- I need to generate a ByteString stream with the first parser, which is the input for
-- the second parser. 
-- 
-- The first parser wait until the second consume the previous chunk, so it is pull-based.
--
-- many parsing stages can be chained with this operator.
--
-- The output is nondeterministic: it can return 0, 1 or more results
--
-- example: https://t.co/fmx1uE2SUd
(|-) :: TransIO (StreamData BS.ByteString) -> TransIO b -> TransIO b
p |- q =  do
   v  <- liftIO $ newEmptyMVar
   initp v <|> initq v

 where
 initq v= do
   --abduce
   setParseStream (takeMVar v >>= \v -> (return v !> ("!- operator return",v)))  -- each time the parser need more data, takes the var
   q 
   
 initp v=  abduce >> repeatIt
   where
   repeatIt= (do r <- p; liftIO  (putMVar v r !> "putMVar") ; empty) <|> repeatIt 

