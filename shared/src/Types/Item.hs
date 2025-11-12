{-# LANGUAGE DeriveGeneric #-}

-- | Định nghĩa cấu trúc dữ liệu cho Vật phẩm (Item)
module Types.Item where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Types.Common (EntityID, Vector2D)

data ItemType = HealthPotion | ManaPotion | UpgradeShard deriving (Show, Eq, Generic)

instance ToJSON ItemType
instance FromJSON ItemType

data WorldItem = WorldItem
  { itemId   :: EntityID
  , itemType :: ItemType
  , itemPos  :: Vector2D
  } deriving (Show, Eq, Generic)

instance ToJSON WorldItem
instance FromJSON WorldItem
