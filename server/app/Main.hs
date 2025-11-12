module Main where

import Control.Concurrent (forkIO)
import qualified Network.WebSockets as WS
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import ServerApp (application, gameLoop, initServerState)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "[Main] Starting Arcane Duelists Server..."

  serverState <- initServerState

  putStrLn "[Main] Spawning game loop (60 Hz)"
  _ <- forkIO $ gameLoop serverState

  let host = "127.0.0.1"
      port = 9160

  putStrLn $ "[Main] WebSocket listening on " ++ host ++ ":" ++ show port
  WS.runServer host port (application serverState)