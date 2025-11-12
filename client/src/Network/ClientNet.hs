{-# LANGUAGE OverloadedStrings #-}

module Network.ClientNet
    ( ClientNetEnv(..)
    , networkLoop
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, when)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Network.WebSockets as WS

import Core.Animation (Animation(..), AnimationState(..))
import Renderer.Resources (ResourceMap, emptyAnimation)
import Types.Common (EntityID)
import Types.GameTypes (GameState(..))
import Network.Protocol (ServerMessage(..))

data ClientNetEnv = ClientNetEnv
    { cneGame      :: TVar GameState
    , cneAnims     :: TVar (Map.Map EntityID AnimationState)
    , cneResources :: ResourceMap
    , cneMyId      :: TVar (Maybe EntityID)
    , cneConn      :: TVar (Maybe WS.Connection)
    , cneRunning   :: TVar Bool
    , cneMessageQueue :: TQueue ServerMessage
    }

networkLoop :: ClientNetEnv -> String -> Int -> IO ()
networkLoop env host port = loop
    where
        loop = do
            result <- try (WS.runClient host port "/" (clientSession env)) :: IO (Either SomeException ())
            case result of
                Left err -> do
                    putStrLn $ "[ClientNet] Connection error: " ++ show err
                    atomically $ writeTVar (cneConn env) Nothing
                    stillRunning <- readTVarIO (cneRunning env)
                    when stillRunning $ do
                        threadDelay reconnectDelay
                        loop
                Right _ -> pure ()

        reconnectDelay = 1 * 1000 * 1000 -- 1s pause before reconnect attempts

clientSession :: ClientNetEnv -> WS.ClientApp ()
clientSession env conn =
    finally session cleanup
    where
        session = do
            atomically $ writeTVar (cneConn env) (Just conn)
            forever $ do
                msg <- WS.receiveData conn
                case Aeson.decode msg of
                    Just (SM_AssignPlayerID pid) ->
                        atomically $ writeTVar (cneMyId env) (Just pid)
                    Just (SM_UpdateGameState newWorld) ->
                        atomically $ do
                            writeTVar (cneGame env) newWorld
                            syncAnimationMapSTM (cneAnims env) (cneResources env) newWorld
                    Just loginMsg@(SM_LoginResponse _) ->
                        atomically $ writeTQueue (cneMessageQueue env) loginMsg
                    Nothing -> pure ()

        cleanup = atomically $ writeTVar (cneConn env) Nothing

syncAnimationMapSTM :: TVar (Map.Map EntityID AnimationState) -> ResourceMap -> GameState -> STM ()
syncAnimationMapSTM animVar res gs = do
    current <- readTVar animVar
    let players = Map.keys (gsPlayers gs)
        withAll = foldr ensure current players
        ensure pid acc =
            if Map.member pid acc
                then acc
                else Map.insert pid (defaultAnimState res) acc
        trimmed = Map.filterWithKey (\pid _ -> Map.member pid (gsPlayers gs)) withAll
    writeTVar animVar trimmed

defaultAnimState :: ResourceMap -> AnimationState
defaultAnimState res = AnimationState defaultName (lookupAnim defaultName) 0 0 False
    where
        defaultName = "mage_idle_down"
        lookupAnim name = Map.findWithDefault emptyAnimation name res