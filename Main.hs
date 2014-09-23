import Control.Applicative ((<$>))
import Control.Concurrent (newMVar)
import Control.Exception
import Network.WebSockets (runServer)
import System.Environment (getEnv)
import Collab.App (app)
import Collab.State (new)

getPort :: Int -> IO Int
getPort d = (read <$> getEnv "PORT") `catch` \(SomeException _) -> return d

main :: IO ()
main = do
  port  <- getPort 9000
  state <- new
  putStrLn $ "Listening on port " ++ show port
  runServer "127.0.0.1" port $ app state
