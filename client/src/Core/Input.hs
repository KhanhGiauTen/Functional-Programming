-- {-- FILE: GAMEDEV/client/src/Core/Input.hs --}
-- Xử lý input (bàn phím, chuột)

module Core.Input
    ( handleEvents
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Linear (V2(..))
import qualified SDL

import Types.Common (Vector2D(..), isZeroVec)
import Types.NetworkTypes (PlayerInput(..), SpellCastType(..))
import Types.Config (halfScreenWidth, halfScreenHeight)

-- | Poll SDL events and push translated inputs into the provided queue.
handleEvents :: [SDL.Event] -> TQueue PlayerInput -> TVar Bool -> IO ()
handleEvents events inputQueue runningVar = do
    keyboard <- SDL.getKeyboardState

    let eventInputs = events >>= eventToInputs . SDL.eventPayload
        moveInputs  = movementInputs keyboard
        inputs = eventInputs ++ moveInputs
        shouldQuit = any isQuitEvent events

    atomically $ forM_ inputs (writeTQueue inputQueue)
    when shouldQuit $ atomically $ writeTVar runningVar False

eventToInputs :: SDL.EventPayload -> [PlayerInput]
eventToInputs (SDL.KeyboardEvent keyEvent)
    | SDL.keyboardEventKeyMotion keyEvent /= SDL.Pressed = []
    | otherwise =
        case SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent) of
            SDL.KeycodeQ -> [InputCastSpell CastSkill1]
            SDL.KeycodeR -> [InputCastSpell CastUltimate]
            _            -> []
eventToInputs (SDL.MouseButtonEvent btnEvent)
    | SDL.mouseButtonEventMotion btnEvent /= SDL.Pressed = []
    | SDL.mouseButtonEventButton btnEvent /= SDL.ButtonLeft = []
    | otherwise =
        let SDL.P (V2 x y) = SDL.mouseButtonEventPos btnEvent
            vecX = fromIntegral x - halfScreenWidth
            vecY = halfScreenHeight - fromIntegral y
        in [InputCastSpell (CastBasicAttack (Vec vecX vecY))]
eventToInputs _ = []

isQuitEvent :: SDL.Event -> Bool
isQuitEvent ev =
    case SDL.eventPayload ev of
        SDL.QuitEvent -> True
        SDL.KeyboardEvent keyEvent ->
            SDL.keyboardEventKeyMotion keyEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent) == SDL.KeycodeEscape
        _ -> False

movementInputs :: (SDL.Scancode -> Bool) -> [PlayerInput]
movementInputs keyboardState =
    case movementVector keyboardState of
      vec | isZeroVec vec -> []
          | otherwise     -> [InputMove vec]

movementVector :: (SDL.Scancode -> Bool) -> Vector2D
movementVector keyboardState = Vec horiz vert
  where
    horiz = boolToFloat (keyboardState SDL.ScancodeD) - boolToFloat (keyboardState SDL.ScancodeA)
    vert  = boolToFloat (keyboardState SDL.ScancodeW) - boolToFloat (keyboardState SDL.ScancodeS)

    boolToFloat :: Bool -> Float
    boolToFloat True  = 1
    boolToFloat False = 0