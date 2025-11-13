{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Định nghĩa cấu trúc dữ liệu cho Quái vật (Enemy)
module Types.Enemy where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Types.Common (EntityID, Vector2D, EntityState, Direction)

data EnemyType = Golem | Elemental | Ghost deriving (Show, Eq, Generic, NFData)

instance ToJSON EnemyType
instance FromJSON EnemyType

data AIState = AI_Idle | AI_Chasing EntityID | AI_Attacking EntityID deriving (Show, Eq, Generic, NFData)

instance ToJSON AIState
instance FromJSON AIState

data Enemy = Enemy
  { enemyId      :: EntityID
  , enemyType    :: EnemyType
  , enemyPos     :: Vector2D
  , enemyState   :: EntityState
  , enemyDir     :: Direction
  , enemyHealth  :: Int
  , maxEnemyHealth :: Int
  , enemyTarget  :: Maybe EntityID
  , enemyAIState :: AIState
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON Enemy
instance FromJSON Enemy
