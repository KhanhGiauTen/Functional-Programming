module Systems.DungeonSystem
    ( spawnRandomEnemies
    ) where

import qualified Data.Map as Map
import System.Random (StdGen, randomR)
import Types.Common (EntityID, Vector2D(..), Direction(..), EntityState(..))
import Systems.EntitySystem (World)
import Types.GameTypes (GameState(..))
import Types.Enemy (Enemy(..), EnemyType(..), AIState(..))
import Types.Config
    ( enemySpawnGroundY
    , enemySpawnMargin
    , enemySpawnVerticalJitter
    , halfScreenWidth
    )

-- | Hàm sinh ID mới dựa trên số lượng quái hiện có
getNewEnemyID :: World -> EntityID
getNewEnemyID world = 100 + Map.size (gsEnemies world)

spawnRandomEnemies :: Int -> StdGen -> World -> (World, StdGen)
spawnRandomEnemies count gen world = go count gen world
    where
        go :: Int -> StdGen -> World -> (World, StdGen)
        go 0 g w = (w, g)
        go n g w =
            let
                (typeRoll, g1) = randomR (0 :: Int, 2) g
                enemyType = case typeRoll of
                    0 -> Golem
                    1 -> Elemental
                    _ -> Ghost
                (sideRoll, g2) = randomR (0 :: Int, 1) g1
                spawnX =
                  if sideRoll == 0
                    then (-halfScreenWidth - enemySpawnMargin)
                    else (halfScreenWidth + enemySpawnMargin)
                (spawnOffsetY, g3) = randomR (-enemySpawnVerticalJitter, enemySpawnVerticalJitter) g2
                spawnY = enemySpawnGroundY + spawnOffsetY
                spawnDir = if sideRoll == 0 then DirRight else DirLeft
                health = case enemyType of
                    Golem     -> 90
                    Elemental -> 70
                    Ghost     -> 55
                enemyId = getNewEnemyID w
                newEnemy = Enemy
                    { enemyId = enemyId
                    , enemyType = enemyType
                    , enemyPos = Vec spawnX spawnY
                    , enemyState = Walking
                    , enemyDir = spawnDir
                    , enemyHealth = health
                    , maxEnemyHealth = health
                    , enemyTarget = Nothing
                    , enemyAIState = AI_Idle
                    }
                enemies' = Map.insert enemyId newEnemy (gsEnemies w)
                world' = w { gsEnemies = enemies' }
            in go (n - 1) g3 world'