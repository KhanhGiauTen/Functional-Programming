-- {-- FILE: GAMEDEV/client/src/Renderer/Resources.hs --}
-- Nạp tài nguyên (PNG, âm thanh)

module Renderer.Resources
  ( ResourceMap
  , loadAllResources
  , emptyAnimation
  ) where

import Control.Exception (try)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified SDL
import qualified SDL.Image as SDL.Image
import SDL.Exception (SDLException)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)

import Core.Animation (Animation(..))

type ResourceMap = Map.Map String Animation

emptyAnimation :: Animation
emptyAnimation = Animation [] 0 False

loadAllResources :: SDL.Renderer -> IO ResourceMap
loadAllResources renderer = do
  putStrLn "[Resources] Loading UI textures..."

  healthFillTex  <- loadTexture renderer "client/assets/textures/ui/health_bar.png"
  healthFrameTex <- loadTexture renderer "client/assets/textures/ui/empty_bar.png"
  manaFillTex    <- loadTexture renderer "client/assets/textures/ui/magic_bar.png"
  bgLayer1Tex    <- loadTexture renderer "client/assets/textures/ui/background/background_layer_1.png"
  bgLayer2Tex    <- loadTexture renderer "client/assets/textures/ui/background/background_layer_2.png"
  bgLayer3Tex    <- loadTexture renderer "client/assets/textures/ui/background/background_layer_3.png"
  tilesetTex     <- loadTexture renderer "client/assets/textures/ui/oak_woods_tileset.png"

  let cultistDir = "client/assets/textures/monsters/Cultist_Priest"
      frostDir   = "client/assets/textures/monsters/FrostIce"

  cultistIdleFrames   <- loadAnimationFramesByPrefix renderer cultistDir "cultist_priest_idle_"
  cultistWalkFrames   <- loadAnimationFramesByPrefix renderer cultistDir "cultist_priest_walk_"
  cultistAttackFrames <- loadAnimationFramesByPrefix renderer cultistDir "cultist_priest_attack_"
  cultistHitFrames    <- loadAnimationFramesByPrefix renderer cultistDir "cultist_priest_takehit_"
  cultistDeathFrames  <- loadAnimationFramesByPrefix renderer cultistDir "cultist_priest_die_"

  frostIdleFrames   <- loadAnimationFramesDir renderer (frostDir </> "idle")
  frostWalkFrames   <- loadAnimationFramesDir renderer (frostDir </> "walk")
  frostAttackFrames <- loadAnimationFramesDir renderer (frostDir </> "1_atk")
  frostHitFrames    <- loadAnimationFramesDir renderer (frostDir </> "take_hit")
  frostDeathFrames  <- loadAnimationFramesDir renderer (frostDir </> "death")

  for_ tilesetTex $ \tex -> do
    info <- SDL.queryTexture tex
    putStrLn $ "[Resources] Tileset size: " ++ show (SDL.textureWidth info) ++ "x" ++ show (SDL.textureHeight info)

  let mkStatic tex = Animation (maybeToList tex) 999 False
      resourcesUI =
        [ ("ui_health_fill", mkStatic healthFillTex)
        , ("ui_health_frame", mkStatic healthFrameTex)
        , ("ui_mana_fill", mkStatic manaFillTex)
        , ("ui_bg_layer_1", mkStatic bgLayer1Tex)
        , ("ui_bg_layer_2", mkStatic bgLayer2Tex)
        , ("ui_bg_layer_3", mkStatic bgLayer3Tex)
        , ("tileset_oak", mkStatic tilesetTex)
        ]
      cultistIdleAnim   = Animation cultistIdleFrames 0.16 True
      cultistWalkAnim   = Animation cultistWalkFrames 0.12 True
      cultistAttackAnim = Animation cultistAttackFrames 0.08 False
      cultistHitAnim    = Animation cultistHitFrames 0.08 False
      cultistDeathAnim  = Animation cultistDeathFrames 0.12 False

      frostIdleAnim   = Animation frostIdleFrames 0.16 True
      frostWalkAnim   = Animation frostWalkFrames 0.10 True
      frostAttackAnim = Animation frostAttackFrames 0.07 False
      frostHitAnim    = Animation frostHitFrames 0.08 False
      frostDeathAnim  = Animation frostDeathFrames 0.12 False

      enemyAnimations =
        [ ("enemy_cultist_idle", cultistIdleAnim)
        , ("enemy_cultist_walk", cultistWalkAnim)
        , ("enemy_cultist_attack", cultistAttackAnim)
        , ("enemy_cultist_hit", cultistHitAnim)
        , ("enemy_cultist_death", cultistDeathAnim)
        , ("enemy_frost_idle", frostIdleAnim)
        , ("enemy_frost_walk", frostWalkAnim)
        , ("enemy_frost_attack", frostAttackAnim)
        , ("enemy_frost_hit", frostHitAnim)
        , ("enemy_frost_death", frostDeathAnim)
        ]

  putStrLn "[Resources] Loading mage animations..."

  mage1IdleFrames   <- loadAnimationFramesSeq renderer "client/assets/textures/mage/Mage_1/Idle/Idle"
  mage1RunFrames    <- loadAnimationFramesSeq renderer "client/assets/textures/mage/Mage_1/Run/Run"
  mage1AttackFrames <- loadAnimationFramesSeq renderer "client/assets/textures/mage/Mage_1/Attack/StaffWood/AttackWood"

  mage2IdleFrames   <- loadAnimationFramesDir renderer "client/assets/textures/mage/Mage_2/idle"
  mage2WalkFrames   <- loadAnimationFramesDir renderer "client/assets/textures/mage/Mage_2/walk"
  mage2AttackFrames <- loadAnimationFramesDir renderer "client/assets/textures/mage/Mage_2/atk"

  when (null mage1IdleFrames)   $ putStrLn "[Resources] Mage_1 idle frames missing"
  when (null mage1RunFrames)    $ putStrLn "[Resources] Mage_1 run frames missing"
  when (null mage1AttackFrames) $ putStrLn "[Resources] Mage_1 attack frames missing"
  when (null mage2IdleFrames)   $ putStrLn "[Resources] Mage_2 idle frames missing"
  when (null mage2WalkFrames)   $ putStrLn "[Resources] Mage_2 walk frames missing"
  when (null mage2AttackFrames) $ putStrLn "[Resources] Mage_2 attack frames missing"
  when (null cultistIdleFrames)   $ putStrLn "[Resources] Cultist idle frames missing"
  when (null cultistWalkFrames)   $ putStrLn "[Resources] Cultist walk frames missing"
  when (null cultistAttackFrames) $ putStrLn "[Resources] Cultist attack frames missing"
  when (null cultistHitFrames)    $ putStrLn "[Resources] Cultist hit frames missing"
  when (null cultistDeathFrames)  $ putStrLn "[Resources] Cultist death frames missing"
  when (null frostIdleFrames)     $ putStrLn "[Resources] Frost idle frames missing"
  when (null frostWalkFrames)     $ putStrLn "[Resources] Frost walk frames missing"
  when (null frostAttackFrames)   $ putStrLn "[Resources] Frost attack frames missing"
  when (null frostHitFrames)      $ putStrLn "[Resources] Frost hit frames missing"
  when (null frostDeathFrames)    $ putStrLn "[Resources] Frost death frames missing"

  let mage1IdleAnim   = Animation mage1IdleFrames 0.18 True
      mage1RunAnim    = Animation mage1RunFrames 0.12 True
      mage1AttackAnim = Animation mage1AttackFrames 0.09 False

      mage2IdleAnim   = Animation mage2IdleFrames 0.18 True
      mage2WalkAnim   = Animation mage2WalkFrames 0.12 True
      mage2AttackAnim = Animation mage2AttackFrames 0.09 False

  putStrLn "[Resources] Done."

  let animEntries =
        [ ("mage_idle_down", mage1IdleAnim)
        , ("mage_idle_up", mage1IdleAnim)
        , ("mage_idle_left", mage1IdleAnim)
        , ("mage_idle_right", mage1IdleAnim)
        , ("mage_walk_down", mage1RunAnim)
        , ("mage_walk_up", mage1RunAnim)
        , ("mage_walk_left", mage1RunAnim)
        , ("mage_walk_right", mage1RunAnim)
        , ("mage_attack_down", mage1AttackAnim)
        , ("mage_attack_up", mage1AttackAnim)
        , ("mage_attack_left", mage1AttackAnim)
        , ("mage_attack_right", mage1AttackAnim)

        , ("mage1_idle_down", mage1IdleAnim)
        , ("mage1_idle_up", mage1IdleAnim)
        , ("mage1_idle_left", mage1IdleAnim)
        , ("mage1_idle_right", mage1IdleAnim)
        , ("mage1_walk_down", mage1RunAnim)
        , ("mage1_walk_up", mage1RunAnim)
        , ("mage1_walk_left", mage1RunAnim)
        , ("mage1_walk_right", mage1RunAnim)
        , ("mage1_attack_down", mage1AttackAnim)
        , ("mage1_attack_up", mage1AttackAnim)
        , ("mage1_attack_left", mage1AttackAnim)
        , ("mage1_attack_right", mage1AttackAnim)

        , ("mage2_idle_down", mage2IdleAnim)
        , ("mage2_idle_up", mage2IdleAnim)
        , ("mage2_idle_left", mage2IdleAnim)
        , ("mage2_idle_right", mage2IdleAnim)
        , ("mage2_walk_down", mage2WalkAnim)
        , ("mage2_walk_up", mage2WalkAnim)
        , ("mage2_walk_left", mage2WalkAnim)
        , ("mage2_walk_right", mage2WalkAnim)
        , ("mage2_attack_down", mage2AttackAnim)
        , ("mage2_attack_up", mage2AttackAnim)
        , ("mage2_attack_left", mage2AttackAnim)
        , ("mage2_attack_right", mage2AttackAnim)
        ]

  let baseMap = Map.fromList resourcesUI
      enemyMap = Map.fromList enemyAnimations
      animMap = Map.fromList animEntries

  pure $ Map.unions [baseMap, enemyMap, animMap]

loadTexture :: SDL.Renderer -> FilePath -> IO (Maybe SDL.Texture)
loadTexture renderer path = do
  result <- try (SDL.Image.loadTexture renderer path) :: IO (Either SDLException SDL.Texture)
  case result of
    Left _  -> putStrLn ("[Resources] Missing asset: " ++ path) >> pure Nothing
    Right t -> pure (Just t)

loadAnimationFramesSeq :: SDL.Renderer -> FilePath -> IO [SDL.Texture]
loadAnimationFramesSeq renderer base = go 1
  where
    go idx = do
      let path = base ++ show idx ++ ".png"
      result <- try (SDL.Image.loadTexture renderer path) :: IO (Either SDLException SDL.Texture)
      case result of
        Left _ -> if idx == 1
                    then putStrLn ("[Resources] Missing asset: " ++ path) >> pure []
                    else pure []
        Right tex -> do
          rest <- go (idx + 1)
          pure (tex : rest)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

loadAnimationFramesDir :: SDL.Renderer -> FilePath -> IO [SDL.Texture]
loadAnimationFramesDir renderer dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then putStrLn ("[Resources] Missing directory: " ++ dir) >> pure []
    else do
      files <- listDirectory dir
      let pngFiles = sortOn naturalSortKey [ dir </> file | file <- files, takeExtension file == ".png" ]
      loadTexturesFromPaths renderer pngFiles

loadAnimationFramesByPrefix :: SDL.Renderer -> FilePath -> String -> IO [SDL.Texture]
loadAnimationFramesByPrefix renderer dir prefix = do
  exists <- doesDirectoryExist dir
  if not exists
    then putStrLn ("[Resources] Missing directory: " ++ dir) >> pure []
    else do
      files <- listDirectory dir
      let matches = [ dir </> file
                    | file <- files
                    , takeExtension file == ".png"
                    , prefix `isPrefixOf` takeBaseName file
                    ]
      loadTexturesFromPaths renderer (sortOn naturalSortKey matches)

loadTexturesFromPaths :: SDL.Renderer -> [FilePath] -> IO [SDL.Texture]
loadTexturesFromPaths renderer paths = catMaybes <$> mapM loadFrame paths
  where
    loadFrame path = do
      result <- try (SDL.Image.loadTexture renderer path) :: IO (Either SDLException SDL.Texture)
      case result of
        Left _  -> putStrLn ("[Resources] Missing asset: " ++ path) >> pure Nothing
        Right t -> pure (Just t)

naturalSortKey :: FilePath -> (String, Int)
naturalSortKey path =
  let base = takeBaseName path
      revBase = reverse base
      digitsRev = takeWhile isDigit revBase
      digitsStr = reverse digitsRev
      prefix = reverse (dropWhile isDigit revBase)
      order = if null digitsStr then 0 else read digitsStr
  in (prefix, order)