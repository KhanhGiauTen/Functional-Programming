{-# LANGUAGE DeriveGeneric #-}

-- | Giao thức giao tiếp chính (Client <-> Server)
module Network.Protocol where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Types.GameTypes (GameState)
import Types.NetworkTypes (PlayerInput)
import Types.Player (PlayerMage)
import Types.Account (LoginRequest, LoginResponse)
import Types.Common (EntityID)

-- | Tin nhắn Client gửi lên Server
data ClientMessage
  = CM_Login LoginRequest
  | CM_Input PlayerInput
  | CM_SelectMage PlayerMage
  deriving (Show, Eq, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage

-- | Tin nhắn Server gửi xuống Client
data ServerMessage
  = SM_LoginResponse LoginResponse
  | SM_UpdateGameState GameState
  | SM_AssignPlayerID EntityID
  deriving (Show, Eq, Generic)

instance ToJSON ServerMessage
instance FromJSON ServerMessage


