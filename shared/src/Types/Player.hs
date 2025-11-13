{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Định nghĩa cấu trúc dữ liệu cho Người chơi
module Types.Player where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Types.Common (EntityID, Vector2D, EntityState, Direction)

-- | Which mage avatar the player uses.
data PlayerMage
  = Mage1
  | Mage2
  deriving (Show, Eq, Generic, NFData)

instance ToJSON PlayerMage
instance FromJSON PlayerMage

data Player = Player
  { playerId          :: EntityID
  , playerPos         :: Vector2D
  , playerState       :: EntityState
  , playerDir         :: Direction
  , playerHealth      :: Int
  , maxHealth         :: Int
  , playerMana        :: Int
  , maxMana           :: Int
  , playerLives       :: Int
  , skill1Cooldown    :: Float
  , ultimateCooldown  :: Float
  , playerAttackTimer :: Float
  , playerMage        :: PlayerMage
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON Player
instance FromJSON Player
