{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.GameTypes (
    GameState(..)
  , Player(..)
  , Projectile(..)
  , Enemy
  , initialGameState
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Map as Map

import Types.Common (EntityID, Vector2D)
import Types.Player (Player(..))
import qualified Types.Enemy as EN

type Enemy = EN.Enemy

data Projectile = Projectile
  { projId       :: EntityID
  , projOwnerId  :: EntityID
  , projPos      :: Vector2D
  , projVelocity :: Vector2D
  , projDamage   :: Int
  , projLifetime :: Float
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON Projectile
instance ToJSON Projectile

data WorldItem = WorldItem
  { itemId :: EntityID
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON WorldItem
instance ToJSON WorldItem

data GameState = GameState
  { gsPlayers     :: Map.Map EntityID Player
  , gsEnemies     :: Map.Map EntityID Enemy
  , gsProjectiles :: Map.Map EntityID Projectile
  , gsItems       :: Map.Map EntityID WorldItem
  , gsRespawns    :: Map.Map EntityID Float
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON GameState
instance ToJSON GameState

initialGameState :: GameState
initialGameState = GameState Map.empty Map.empty Map.empty Map.empty Map.empty