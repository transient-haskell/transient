
import           Transient.Base
import           Network
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative

-- taken from  Pipes example
-- https://www.reddit.com/r/haskell/comments/2jvc78/simple_haskell_irc_client_in_two_lines_of_code/?st=iqj5yxg1&sh=0cb8cc11
-- Simple Haskell IRC client in "two lines of code"
--
--main = withSocketsDo $ connect "irc.freenode.net" "6667" $ \(s, _) ->
--    forkIO (runEffect $ PBS.stdin >-> toSocket s) >> runEffect (fromSocket s 4096 >-> PBS.stdout)


main = do
    h <- withSocketsDo $ connectTo "irc.freenode.net" $ PortNumber $ fromIntegral 6667
    keep' $ (waitEvents getLine >>= liftIO . hPutStrLn h) <|> ( threads 1 $ waitEvents (hGetLine h) >>= liftIO . putStrLn )
