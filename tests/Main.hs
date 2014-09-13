import Control.Exception (bracket)
import Control.Concurrent (forkIO, killThread)
import Collab.Test.Api (specs)
import Collab.Test.Util

main :: IO ()
main = bracket (forkIO runServerApp) killThread (const specs)
