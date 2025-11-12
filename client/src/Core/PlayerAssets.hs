module Core.PlayerAssets
  ( spritePrefix
  , animationKeyFor
  ) where

import Data.List (isPrefixOf)

import Types.Player (Player(..), PlayerMage(..))

spritePrefix :: Player -> String
spritePrefix player =
  case playerMage player of
    Mage1 -> "mage1"
    Mage2 -> "mage2"

animationKeyFor :: Player -> String -> String
animationKeyFor player baseName
  | "mage" `isPrefixOf` baseName = prefix ++ drop 4 baseName
  | otherwise                    = prefix ++ "_" ++ baseName
  where
    prefix = spritePrefix player
