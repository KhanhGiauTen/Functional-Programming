-- | Các kiểu dữ liệu chỉ dùng cho giao tiếp CSDL
module Types.DBTypes where

import Data.Text (Text)
import Data.Int (Int64)

-- | Dữ liệu người dùng lấy từ CSDL
data UserRecord = UserRecord
  { urUserId :: Int64
  , urUsername :: Text
  , urPasswordHash :: Text
  } deriving (Show, Eq)
