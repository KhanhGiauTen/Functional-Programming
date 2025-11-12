-- | Định nghĩa Schema CSDL (dùng cho 'persistent')
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Types.Models where

import Database.Persist.TH
import Data.Text (Text)

-- Định nghĩa các bảng
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    passwordHash Text
    UniqueUsername username
    deriving Show Eq

PlayerCharacter
    userId UserId
    characterName Text
    health Int
    mana Int
    posX Double  -- <-- ĐÃ SỬA: Từ Float sang Double
    posY Double  -- <-- ĐÃ SỬA: Từ Float sang Double
    deriving Show Eq
|]