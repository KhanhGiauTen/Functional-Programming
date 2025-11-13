module Systems.PhysicsSystem
  ( tick
  ) where

import qualified Data.Map as Map
import Types.Common (Vector2D(..), vAdd, vScale)
import Systems.EntitySystem (World) -- ĐÃ SỬA: Import World từ EntitySystem
import Types.GameTypes (Projectile(..), GameState(..)) -- Import GameState để truy cập fields
import Control.Parallel.Strategies (parList, rdeepseq, using)
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor
import qualified Prelude as P
import Prelude hiding (map)

-- | Advance every projectile by dt seconds and remove ones whose lifetime expires.
tick :: Float -> World -> World
tick dt world = cleanupProjectiles moved
  where
    -- Parallel map over projectiles for position/lifetime updates
    movedProjectilesList =
      (P.map (advanceProjectile dt) (Map.elems (gsProjectiles world))) `using` (parList rdeepseq)
    moved = world { gsProjectiles = Map.fromList (zip (Map.keys (gsProjectiles world)) movedProjectilesList) }

advanceProjectile :: Float -> Projectile -> Projectile
advanceProjectile dt projectile =
  let newPos = projPos projectile `vAdd` (dt `vScale` projVelocity projectile)
      newLifetime = projLifetime projectile - dt
  in projectile { projPos = newPos, projLifetime = newLifetime }

cleanupProjectiles :: World -> World
cleanupProjectiles world =
  let active = Map.filter (\p -> projLifetime p > 0) (gsProjectiles world)
  in world { gsProjectiles = active }