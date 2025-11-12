{-# LANGUAGE DeriveGeneric #-}

-- | Các kiểu dữ liệu liên quan đến Tài khoản (đăng nhập)
module Types.Account where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

data LoginRequest = LoginRequest
  { lrUsername :: Text
  , lrPassword :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON LoginRequest
instance FromJSON LoginRequest

data LoginResponse = LoginResponse
  { lrSuccess :: Bool
  , lrMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON LoginResponse
instance FromJSON LoginResponse
