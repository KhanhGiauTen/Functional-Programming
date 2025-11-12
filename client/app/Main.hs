{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Control.Monad (void, forever)
import Data.Maybe (fromMaybe)

import Types.Common (EntityID, Vector2D(..), normalizeVec, vx, vy)
import Types.GameTypes (GameState, initialGameState)
import Types.Player (Player(..))
import Types.NetworkTypes (PlayerInput(..), SpellCastType(..))
import Types.Account (LoginRequest(..), LoginResponse(..))
import Network.Protocol (ClientMessage(..), ServerMessage(..))

import Core.Animation (AnimationState, updateAnimation)
import Renderer.Resources (ResourceMap, loadAllResources)
import Core.Renderer (ClientWorld(..))
import qualified Core.Renderer as Renderer

-- Client application world kept in TVars so both network and render threads can access
data AppWorld = AppWorld
  { awGame    :: TVar GameState
  , awAnims   :: TVar (Map.Map EntityID AnimationState)
  , awRes     :: ResourceMap
  , awConnVar :: MVar WS.Connection
  , awMyId    :: TVar (Maybe EntityID)
  }

main :: IO ()
main = do
  putStrLn "Loading resources..."
  resources <- loadAllResources

  putStrLn "Connecting to server..."
  WS.runClient "127.0.0.1" 9160 "/" $ \conn -> do
    putStrLn "Connected to server. Waiting for Player ID..."

    gameVar <- newTVarIO initialGameState
    animVar <- newTVarIO Map.empty
    connVar <- newMVar conn
    myIdVar <- newTVarIO Nothing

    let app = AppWorld gameVar animVar resources connVar myIdVar

    sendLogin conn

    void $ forkIO $ networkLoop conn app

    playIO (InWindow "Arcane Duelists" (800,600) (10,10)) black 60 app renderClient handleInputClient updateClient

-- | Inform the server about this client session
sendLogin :: WS.Connection -> IO ()
sendLogin conn = do
  let loginReq = LoginRequest "guest" "guest"
  WS.sendTextData conn (Aeson.encode (CM_Login loginReq))

-- | Network listener: update GameState TVar when server sends updates
networkLoop :: WS.Connection -> AppWorld -> IO ()
networkLoop conn app = forever $ do
  msg <- WS.receiveData conn
  case Aeson.decode msg of
    Just (SM_UpdateGameState gs) -> atomically $ writeTVar (awGame app) gs
    Just (SM_AssignPlayerID pid) -> atomically $ writeTVar (awMyId app) (Just pid)
    Just (SM_LoginResponse (LoginResponse ok info)) ->
      putStrLn $ "[Client] Login response: " ++ (if ok then "OK - " else "FAIL - ") ++ show info
    _ -> putStrLn "[Client] Received unknown server message"

-- | Render wrapper that adapts to Core.Renderer.renderGame signature
renderClient :: AppWorld -> IO Picture
renderClient app = do
  gs <- readTVarIO (awGame app)
  anims <- readTVarIO (awAnims app)
  mMyId <- readTVarIO (awMyId app)
  let clientWorld = ClientWorld { cwGame = gs, cwAnims = anims }
      myId = fromMaybe (-1) mMyId
  Renderer.renderGame (awRes app) clientWorld myId

-- | Handle input and send messages to server
handleInputClient :: Event -> AppWorld -> IO AppWorld
handleInputClient ev app = do
  case ev of
    EventKey (Char 'w') Down _ _ -> sendMoveInput app (Vec 0 1)
    EventKey (Char 'a') Down _ _ -> sendMoveInput app (Vec (-1) 0)
    EventKey (Char 's') Down _ _ -> sendMoveInput app (Vec 0 (-1))
    EventKey (Char 'd') Down _ _ -> sendMoveInput app (Vec 1 0)
    EventKey (Char 'q') Down _ _ -> sendSpellInput app CastSkill1
    EventKey (Char 'r') Down _ _ -> sendSpellInput app CastUltimate
    EventKey (MouseButton LeftButton) Down _ (mx, my) -> sendBasicAttack app mx my
    _ -> pure ()
  return app

-- | Update client-side animations each frame
updateClient :: Float -> AppWorld -> IO AppWorld
updateClient dt app = do
  atomically $ modifyTVar' (awAnims app) (Map.map (updateAnimation dt))
  return app

sendMoveInput :: AppWorld -> Vector2D -> IO ()
sendMoveInput app dir =
  withConnection app $ \conn ->
    WS.sendTextData conn (Aeson.encode (CM_Input (InputMove dir)))

sendSpellInput :: AppWorld -> SpellCastType -> IO ()
sendSpellInput app spell =
  withConnection app $ \conn ->
    WS.sendTextData conn (Aeson.encode (CM_Input (InputCastSpell spell)))

sendBasicAttack :: AppWorld -> Float -> Float -> IO ()
sendBasicAttack app mx my = do
  mPid <- readTVarIO (awMyId app)
  case mPid of
    Nothing -> putStrLn "[Client] Waiting for player ID; cannot cast yet."
    Just pid -> do
      gs <- readTVarIO (awGame app)
      case Map.lookup pid (gsPlayers gs) of
        Nothing -> putStrLn "[Client] Player entity not spawned yet."
        Just pl -> do
          let Vec px py = playerPos pl
              dir = normalizeVec (Vec (mx - px) (my - py))
          sendSpellInput app (CastBasicAttack dir)

withConnection :: AppWorld -> (WS.Connection -> IO ()) -> IO ()
withConnection app action = withMVar (awConnVar app) action
