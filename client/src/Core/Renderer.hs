-- {-- FILE: GAMEDEV/client/src/Core/Renderer.hs --}
-- SDL-based renderer for the client world

module Core.Renderer
  ( ClientWorld(..)
  , renderGame
  , enemyAnimationKey
  ) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Linear (V2(..), V4(..))
import qualified SDL

import Core.Animation (Animation(..), AnimationState, getCurrentFrame, getAnimationName)
import Core.PlayerAssets (animationKeyFor)
import Renderer.Resources (ResourceMap)
import Systems.MapLoader (TileLayer(..), TileMap(..), layerRows)
import Types.Common (EntityID, Vector2D(..), EntityState(..))
import Types.GameTypes (GameState(..), Projectile(..))
import Types.Enemy (Enemy(..), EnemyType(..))
import Types.Player (Player(..))
import Types.Config
  ( enemySpriteSize
  , halfScreenHeight
  , halfScreenWidth
  , mageSpriteSize
  , mageNonAttackScale
  , screenHeight
  , screenWidth
  , mageAttackRadius
  )

data ClientWorld = ClientWorld
  { cwGame           :: GameState
  , cwPlayerAnims    :: Map.Map EntityID AnimationState
  , cwEnemyAnims     :: Map.Map EntityID AnimationState
  , cwMap            :: Maybe TileMap
  , cwSelectedPrefix :: Maybe String  -- e.g., Just "mage1" or Just "mage2" for my player
  }

screenWidthInt :: Int
screenWidthInt = round screenWidth

screenHeightInt :: Int
screenHeightInt = round screenHeight

screenWidthC, screenHeightC :: CInt
screenWidthC = fromIntegral screenWidthInt
screenHeightC = fromIntegral screenHeightInt

renderGame :: SDL.Renderer -> ResourceMap -> ClientWorld -> Maybe EntityID -> IO ()
renderGame renderer res cw mMyId = do
  drawBackgroundLayer "ui_bg_layer_1"
  drawBackgroundLayer "ui_bg_layer_2"
  drawBackgroundLayer "ui_bg_layer_3"
  renderTileMap renderer res (cwMap cw)

  let players = gsPlayers (cwGame cw)
      enemies = Map.elems (gsEnemies (cwGame cw))
  mapM_ (renderEnemy renderer res (cwEnemyAnims cw)) enemies
  let selPref = cwSelectedPrefix cw
  mapM_ (renderPlayer renderer res mMyId selPref (cwPlayerAnims cw)) (Map.elems players)
  mapM_ (renderProjectile renderer) (Map.elems $ gsProjectiles (cwGame cw))
  -- draw AoE indicator for local player if attacking
  case mMyId >>= (`Map.lookup` players) of
    Nothing      -> pure ()
    Just player  -> do
      drawAoEIfAttacking player
      renderHUD renderer res player
  where
    drawBackgroundLayer key =
      case Map.lookup key res >>= pickFrame of
        Nothing     -> pure ()
        Just tex -> do
          let dest = SDL.Rectangle (SDL.P (V2 0 0)) (V2 screenWidthC screenHeightC)
          SDL.copy renderer tex Nothing (Just dest)

    pickFrame (Animation frames _ _) = if null frames then Nothing else Just (head frames)

    -- Draw a temporary dotted circle showing AoE radius while attacking
    drawAoEIfAttacking :: Player -> IO ()
    drawAoEIfAttacking p =
      case playerState p of
        Attacking -> drawDottedCircle p
        CastingSkill1 -> drawDottedCircle p
        CastingUltimate -> drawDottedCircle p
        _ -> pure ()

    drawDottedCircle :: Player -> IO ()
    drawDottedCircle p = do
      let Vec px py = playerPos p
          cx = round (px + halfScreenWidth)
          cy = round (halfScreenHeight - py)
          r  = round mageAttackRadius :: Int
          dot = 3 :: CInt
          steps = 64 :: Int
          color = V4 0 210 255 255
      SDL.rendererDrawColor renderer SDL.$= color
      mapM_ (drawDot cx cy r dot) [0 .. steps - 1]

    drawDot :: Int -> Int -> Int -> CInt -> Int -> IO ()
    drawDot cx cy r dot idx = do
      let t = fromIntegral idx / fromIntegral rads
          rads = 64 :: Int
          ang = t * 2 * pi :: Double
          x = cx + round (fromIntegral r * cos ang)
          y = cy + round (fromIntegral r * sin ang)
          rect = SDL.Rectangle (SDL.P (V2 (toCInt (x - 1)) (toCInt (y - 1)))) (V2 dot dot)
      SDL.fillRect renderer (Just rect)

renderTileMap :: SDL.Renderer -> ResourceMap -> Maybe TileMap -> IO ()
renderTileMap renderer res maybeMap =
  case Map.lookup "tileset_oak" res >>= pickFrame of
    Nothing -> pure ()
    Just tex ->
      case activeLayer of
        Nothing     -> pure ()
        Just layer  -> drawLayer tex layer
  where
    activeLayer = do
      tileMap <- maybeMap
      listToMaybe (filter tlVisible (tmLayers tileMap))

    drawLayer tex layer =
      let rowsWithIndex = zip [0 ..] (layerRows layer)
      in mapM_ (drawTileRow tex layer) rowsWithIndex

    drawTileRow tex layer (rowIdx, rowTiles) =
      mapM_ (drawTile tex layer rowIdx) (zip [0..] rowTiles)

    drawTile tex layer rowIdx (colIdx, tileId) =
      case tileSource tileId of
        Nothing -> pure ()
        Just srcRect ->
          let destRect = SDL.Rectangle (SDL.P (V2 (tileBaseX layer + colIdxC * tileSize) (tileBaseY layer + rowIdxC * tileSize))) (V2 tileSize tileSize)
          in SDL.copy renderer tex (Just srcRect) (Just destRect)
      where
        colIdxC = fromIntegral colIdx
        rowIdxC = fromIntegral rowIdx

    tileSizeI :: Int
    tileSizeI = 64

    tileSize :: CInt
    tileSize = fromIntegral tileSizeI

    tileBaseX layer = fromIntegral ((screenWidthInt - mapWidth layer) `div` 2)
    tileBaseY layer = fromIntegral (screenHeightInt - mapHeight layer - 64)

    mapWidth layer = tlWidth layer * tileSizeI
    mapHeight layer = tlHeight layer * tileSizeI

renderHUD :: SDL.Renderer -> ResourceMap -> Player -> IO ()
renderHUD renderer res player = do
  -- Responsive health bar: horizontally centered, width scales with screen
  -- width (min 180px, max 440px, default ~35% of screen width)
  renderBar "ui_health_fill" "ui_health_frame" (safeRatio (playerHealth player) (maxHealth player)) originX healthTop
  renderBar "ui_mana_fill"   "ui_health_frame" (safeRatio (playerMana player) (maxMana player))   originX manaTop
  where
    marginI :: Int
    marginI = 16

    -- Compute responsive bar size based on current screen width
    barWidthI :: Int
    barWidthI = max 180 (min 440 (round (0.35 * fromIntegral screenWidthInt)))

    barHeightI :: Int
    barHeightI = 18

    originXI :: Int
    originXI = (screenWidthInt - barWidthI) `div` 2

    healthTopI :: Int
    healthTopI = marginI

    manaTopI :: Int
    manaTopI = marginI + barHeightI + 8

    -- Convert to CInt once
    barWidth, barHeight, originX, healthTop, manaTop :: CInt
    barWidth   = fromIntegral barWidthI
    barHeight  = fromIntegral barHeightI
    originX    = fromIntegral originXI
    healthTop  = fromIntegral healthTopI
    manaTop    = fromIntegral manaTopI

    renderBar fillKey frameKey ratio left top =
      case (lookupTexture fillKey, lookupTexture frameKey) of
        (Just fillTex, Just frameTex) -> do
          let clamped = max 0 (min 1 ratio)
              destRect = SDL.Rectangle (SDL.P (V2 left top)) (V2 barWidth barHeight)
              fillWidthInt = floor (clamped * fromIntegral barWidth) :: Int
              fillWidth = fromIntegral fillWidthInt :: CInt
          when (fillWidth > 0) $ do
            info <- SDL.queryTexture fillTex
            let srcWidthInt = floor (clamped * fromIntegral (SDL.textureWidth info)) :: Int
                srcHeight = fromIntegral (SDL.textureHeight info) :: CInt
            when (srcWidthInt > 0) $ do
              let srcRect = SDL.Rectangle (SDL.P (V2 0 0)) (V2 (fromIntegral srcWidthInt) srcHeight)
                  fillRect = SDL.Rectangle (SDL.P (V2 left top)) (V2 fillWidth barHeight)
              SDL.copy renderer fillTex (Just srcRect) (Just fillRect)
          SDL.copy renderer frameTex Nothing (Just destRect)
        _ -> pure ()

    lookupTexture key = Map.lookup key res >>= pickFrame

tileSource :: Int -> Maybe (SDL.Rectangle CInt)
tileSource tid =
  case tid of
    0 -> Nothing
    1 -> Just $ tileRect (1,5)
    2 -> Just $ tileRect (0,4)
    3 -> Just $ tileRect (1,4)
    4 -> Just $ tileRect (2,4)
    5 -> Just $ tileRect (4,2)
    6 -> Just $ tileRect (3,2)
    7 -> Just $ tileRect (5,2)
    _ -> Just $ tileRect (1,5)
  where
    tileRect (cx, cy) =
      let tileSize = 64 :: CInt
          x = fromIntegral cx * tileSize
          y = fromIntegral cy * tileSize
      in SDL.Rectangle (SDL.P (V2 x y)) (V2 tileSize tileSize)

renderPlayer :: SDL.Renderer -> ResourceMap -> Maybe EntityID -> Maybe String -> Map.Map EntityID AnimationState -> Player -> IO ()
renderPlayer renderer res mMyIdLocal cwSelectedPrefix animStates player =
  case chooseTexture of
    Nothing  -> pure ()
    Just tex -> do
      -- draw player sprite
      SDL.copy renderer tex Nothing (Just destRect)
      -- draw a small HP bar above the player for quick feedback
      drawAboveHeadHP renderer player destRect
  where
    chooseTexture =
      case Map.lookup (playerId player) animStates >>= getCurrentFrame of
        Just tex -> Just tex
        Nothing  -> firstJust [lookupFrame targetKey, lookupFrame targetBase]

    lookupFrame key = Map.lookup key res >>= pickFrame

    firstJust [] = Nothing
    firstJust (x:xs) = case x of
      Nothing -> firstJust xs
      Just v  -> Just v

    targetBase = getAnimationName (playerState player) (playerDir player)
    targetKey = case cwSelectedPrefix of
                  Just pref | isMyPlayer -> pref ++ suffixFromBase targetBase
                  _ -> animationKeyFor player targetBase

    -- Quick helper: from a base like "mage_idle_down" keep the part after "mage"
    suffixFromBase base =
      let dropMage = drop 4 base -- assumes base starts with "mage"
      in if take 4 base == "mage" then dropMage else '_' : base

    isMyPlayer = case mMyIdLocal of
      Nothing -> False
      Just mid -> playerId player == mid


    destRect =
      let Vec px py = playerPos player
          -- choose display size: shrink a bit for Idle/Walking to match Attack size look
          isAttack = case playerState player of
                       Attacking -> True
                       CastingSkill1 -> True
                       CastingUltimate -> True
                       _ -> False
          baseSize :: Float
          baseSize = fromIntegral mageSpriteSize * (if isAttack then 1.0 else mageNonAttackScale)
          sizeI = max 1 (round baseSize)
          halfI = sizeI `div` 2
          screenX = round (px + halfScreenWidth) - halfI
          screenY = round (halfScreenHeight - py) - halfI
      in SDL.Rectangle (SDL.P (V2 (toCInt screenX) (toCInt screenY))) (V2 (toCInt sizeI) (toCInt sizeI))
    -- Local helper: draws an HP bar above the player's head
    drawAboveHeadHP r p (SDL.Rectangle (SDL.P (V2 dx dy)) (V2 w _h)) = do
      let hp = playerHealth p
          mx = maxHealth p
          ratio = if mx <= 0 then 0 else max 0 (min 1 (fromIntegral hp / fromIntegral mx))
          barWidthI = fromIntegral w :: Int
          barHeightI = 6 :: Int
          pad = 1 :: Int
          innerW = max 0 (floor (ratio * fromIntegral (barWidthI - 2*pad))) :: Int
          -- position slightly above the sprite
          bx = dx
          by = dy - toCInt (barHeightI + 4)
          bw = toCInt barWidthI
          bh = toCInt barHeightI
          innerRect = SDL.Rectangle (SDL.P (V2 (bx + toCInt pad) (by + toCInt pad))) (V2 (toCInt innerW) (bh - toCInt (2*pad)))
          outerRect = SDL.Rectangle (SDL.P (V2 bx by)) (V2 bw bh)
      -- background/frame
      SDL.rendererDrawColor r SDL.$= V4 20 20 20 220
      SDL.fillRect r (Just outerRect)
      -- fill color: green -> red based on hp
      let g = floor (255 * ratio) :: Word8
          rCol = floor (255 * (1 - ratio)) :: Word8
      SDL.rendererDrawColor r SDL.$= V4 rCol g 40 240
      when (innerW > 0) $ SDL.fillRect r (Just innerRect)

renderProjectile :: SDL.Renderer -> Projectile -> IO ()
renderProjectile renderer projectile = do
  SDL.rendererDrawColor renderer SDL.$= V4 200 30 30 255
  SDL.fillRect renderer (Just destRect)
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  where
    Vec px py = projPos projectile
    destRect =
      let size = 8 :: Int
          halfSize = size `div` 2
          screenX = round (px + halfScreenWidth) - halfSize
          screenY = round (halfScreenHeight - py) - halfSize
      in SDL.Rectangle (SDL.P (V2 (toCInt screenX) (toCInt screenY))) (V2 (toCInt size) (toCInt size))

renderEnemy :: SDL.Renderer -> ResourceMap -> Map.Map EntityID AnimationState -> Enemy -> IO ()
renderEnemy renderer res animStates enemy =
  case currentTexture of
    Nothing  -> pure ()
    Just tex -> do
      SDL.copy renderer tex Nothing (Just destRect)
      drawAboveHeadHP enemy
  where
    currentTexture =
      case Map.lookup (enemyId enemy) animStates >>= getCurrentFrame of
        Just tex -> Just tex
        Nothing  -> firstJust [Map.lookup key res >>= pickFrame | key <- [enemyAnimationKey enemy, fallbackKey]]

    firstJust [] = Nothing
    firstJust (x:xs) = case x of
      Nothing -> firstJust xs
      Just v  -> Just v

    fallbackKey = enemyBaseKey (enemyType enemy) ++ "_idle"

    spriteSize :: Int
    spriteSize = enemySpriteSize

    destRect =
      let Vec ex ey = enemyPos enemy
          spriteHalf = spriteSize `div` 2
          screenX = round (ex + halfScreenWidth) - spriteHalf
          screenY = round (halfScreenHeight - ey) - spriteHalf
      in SDL.Rectangle (SDL.P (V2 (toCInt screenX) (toCInt screenY))) (V2 spriteSizeC spriteSizeC)

    spriteSizeC = toCInt spriteSize

    drawAboveHeadHP foe = do
      let Vec ex ey = enemyPos foe
          spriteHalf = spriteSize `div` 2
          x0 = round (ex + halfScreenWidth) - spriteHalf
          y0 = round (halfScreenHeight - ey) - spriteHalf
          wI = spriteSize
          barHeightI = 5 :: Int
          pad = 1 :: Int
          bx = toCInt x0
          by = toCInt (y0 - barHeightI - 4)
          bw = toCInt wI
          bh = toCInt barHeightI
          hp = enemyHealth foe
          mx = maxEnemyHealth foe
          ratio = if mx <= 0 then 0 else max 0 (min 1 (fromIntegral hp / fromIntegral mx))
          innerW = max 0 (floor (ratio * fromIntegral (wI - 2*pad))) :: Int
          innerRect = SDL.Rectangle (SDL.P (V2 (bx + toCInt pad) (by + toCInt pad))) (V2 (toCInt innerW) (bh - toCInt (2*pad)))
          outerRect = SDL.Rectangle (SDL.P (V2 bx by)) (V2 bw bh)
          g = floor (255 * ratio) :: Word8
          rCol = floor (255 * (1 - ratio)) :: Word8
      SDL.rendererDrawColor renderer SDL.$= V4 20 20 20 220
      SDL.fillRect renderer (Just outerRect)
      SDL.rendererDrawColor renderer SDL.$= V4 rCol g 40 240
      when (innerW > 0) $ SDL.fillRect renderer (Just innerRect)

enemyAnimationKey :: Enemy -> String
enemyAnimationKey enemy =
  enemyBaseKey (enemyType enemy) ++ enemyStateSuffix (enemyState enemy)

enemyBaseKey :: EnemyType -> String
enemyBaseKey et =
  case et of
    Golem     -> "enemy_frost"
    Elemental -> "enemy_cultist"
    Ghost     -> "enemy_cultist"

enemyStateSuffix :: EntityState -> String
enemyStateSuffix st =
  case st of
    Idle             -> "_idle"
    Walking          -> "_walk"
    Attacking        -> "_attack"
    CastingSkill1    -> "_attack"
    CastingUltimate  -> "_attack"
    Hit              -> "_hit"
    Dead             -> "_death"
    _                -> "_idle"

safeRatio :: Int -> Int -> Float
safeRatio value maxValue
  | maxValue <= 0 = 0
  | otherwise     = clamp (fromIntegral value / fromIntegral maxValue)
  where
    clamp = max 0 . min 1

pickFrame :: Animation -> Maybe SDL.Texture
pickFrame (Animation frames _ _) =
  case frames of
    []    -> Nothing
    (x:_) -> Just x

toCInt :: Int -> CInt
toCInt = fromIntegral