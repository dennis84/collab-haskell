import Control.Concurrent (newMVar)
import qualified Network.WebSockets as WS
import Collab.App (app)

main :: IO ()
main = do
  let p = 9000
  state <- newMVar []
  putStrLn $ "Listening on port " ++ show p
  WS.runServer "127.0.0.1" p $ app state
