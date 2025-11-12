{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SỬA LỖI: Thêm 'isZeroVec' vào danh sách export
module Types.Common (
    EntityID,
    Vector2D(..),
    EntityState(..),
    Direction(..),
    vAdd, vSub, vScale, vMag, normalizeVec, vx, vy,
    isZeroVec -- <-- ĐÃ THÊM VÀO ĐÂY
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

type EntityID = Int

-- Kiểu Vector 2D cơ bản
data Vector2D = Vec Float Float
  deriving (Show, Eq, Generic)

instance FromJSON Vector2D
instance ToJSON Vector2D

-- Hướng nhìn của thực thể
data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq, Generic)

instance FromJSON Direction
instance ToJSON Direction

-- Trạng thái của một thực thể (Entity)
data EntityState
  = Idle
  | Walking
  | Attacking
  | CastingSkill1
  | CastingUltimate
  | Hit
  | Dead
  deriving (Show, Eq, Generic)

instance FromJSON EntityState
instance ToJSON EntityState

-- --- HÀM TOÁN HỌC VECTOR ---

vAdd :: Vector2D -> Vector2D -> Vector2D
(Vec x1 y1) `vAdd` (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)

vSub :: Vector2D -> Vector2D -> Vector2D
(Vec x1 y1) `vSub` (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)

vScale :: Float -> Vector2D -> Vector2D
s `vScale` (Vec x y) = Vec (s * x) (s * y)

vMag :: Vector2D -> Float
vMag (Vec x y) = sqrt (x*x + y*y)

normalizeVec :: Vector2D -> Vector2D
normalizeVec v@(Vec x y) =
  let len = vMag v
  in if len == 0 then Vec 0 0 else (1/len) `vScale` v

vx :: Vector2D -> Float
vx (Vec x _) = x

vy :: Vector2D -> Float
vy (Vec _ y) = y

-- SỬA LỖI: Hàm này đã được định nghĩa, chỉ cần export
isZeroVec :: Vector2D -> Bool
isZeroVec (Vec x y) = x == 0 && y == 0