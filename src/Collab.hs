import Control.Applicative ((<$>))
import Network.WebSockets (runServer)
import System.Environment (getArgs)
import Collab.App (app)
import qualified Collab.State as State

conf :: IO (String, Int)
conf = parse <$> getArgs
  where local = ("127.0.0.1", 9000)
        parse []            = local
        parse (host:[])     = local
        parse (host:port:_) = (host, read port)

main :: IO ()
main = do
  (host, port) <- conf
  state <- State.new
  putStrLn $ "Listening on " ++ host ++ ":" ++ show port
  runServer host port $ app state
