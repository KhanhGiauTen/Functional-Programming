{-# LANGUAGE DeriveGeneric #-}

-- | Các kiểu dữ liệu hỗ trợ cho việc Gửi/Nhận
module Types.NetworkTypes where

import Types.Common (Vector2D)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Các loại kỹ năng
data SpellCastType
  = CastBasicAttack Vector2D -- Hướng bắn
  | CastSkill1
  | CastUltimate
  deriving (Show, Eq, Generic)

instance ToJSON SpellCastType
instance FromJSON SpellCastType

-- | Các loại input từ người chơi
data PlayerInput
  = InputMove Vector2D
  | InputCastSpell SpellCastType
  | InputPickupItems
  deriving (Show, Eq, Generic)

instance ToJSON PlayerInput
instance FromJSON PlayerInput
