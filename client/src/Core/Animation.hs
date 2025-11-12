-- {-- FILE: GAMEDEV/client/src/Core/Animation.hs --}
-- Logic hoạt họa (Animation)

module Core.Animation where

import qualified SDL
import Types.Common (EntityState(..), Direction(..))

-- | Định nghĩa một Animation dựa trên SDL Texture
data Animation = Animation
  { animFrames    :: [SDL.Texture]
  , animFrameRate :: Float
  , animLoops     :: Bool
  }

-- | Trạng thái hiện tại của một animation
data AnimationState = AnimationState
  { animName      :: String
  , animCurrent   :: Animation
  , animTime      :: Float
  , animFrameIdx  :: Int
  , animFinished  :: Bool
  }

-- | Tick animation theo thời gian dt (giây)
updateAnimation :: Float -> AnimationState -> AnimationState
updateAnimation dt animState
  | animFinished animState = animState
  | otherwise =
      let newTime = animTime animState + dt
          frameDuration = animFrameRate (animCurrent animState)
      in if frameDuration <= 0 || null (animFrames (animCurrent animState))
           then animState { animFinished = True }
           else if newTime < frameDuration
                  then animState { animTime = newTime }
                  else advanceFrame (animState { animTime = 0 })
  where
    advanceFrame state =
      let nextIdx = animFrameIdx state + 1
          totalFrames = length (animFrames (animCurrent state))
      in if nextIdx >= totalFrames
            then if animLoops (animCurrent state)
                   then state { animFrameIdx = 0 }
                   else state { animFrameIdx = totalFrames - 1, animFinished = True }
            else state { animFrameIdx = nextIdx }

-- | Lấy frame SDL hiện tại (nếu có)
getCurrentFrame :: AnimationState -> Maybe SDL.Texture
getCurrentFrame animState =
  let frames = animFrames (animCurrent animState)
  in if null frames
        then Nothing
        else Just (frames !! animFrameIdx animState)

-- | Map trạng thái & hướng sang tên animation tương ứng
getAnimationName :: EntityState -> Direction -> String
getAnimationName Idle DirDown  = "mage_idle_down"
getAnimationName Idle DirUp    = "mage_idle_up"
getAnimationName Idle DirLeft  = "mage_idle_left"
getAnimationName Idle DirRight = "mage_idle_right"
getAnimationName Walking DirDown  = "mage_walk_down"
getAnimationName Walking DirUp    = "mage_walk_up"
getAnimationName Walking DirLeft  = "mage_walk_left"
getAnimationName Walking DirRight = "mage_walk_right"
getAnimationName Attacking dir    = "mage_attack_" ++ directionSuffix dir
getAnimationName _ dir            = getAnimationName Idle dir

directionSuffix :: Direction -> String
directionSuffix DirDown  = "down"
directionSuffix DirUp    = "up"
directionSuffix DirLeft  = "left"
directionSuffix DirRight = "right"