module Systems.PhysicsSystem
  ( tick
  ) where

import qualified Data.Map as Map
import Types.Common (Vector2D(..), vAdd, vScale)
import Systems.EntitySystem (World) -- ĐÃ SỬA: Import World từ EntitySystem
import Types.GameTypes (Projectile(..), GameState(..)) -- Import GameState để truy cập fields

-- | Advance every projectile by dt seconds and remove ones whose lifetime expires.
tick :: Float -> World -> World
tick dt world = cleanupProjectiles moved
  where
    moved = world { gsProjectiles = Map.map (advanceProjectile dt) (gsProjectiles world) }

advanceProjectile :: Float -> Projectile -> Projectile
advanceProjectile dt projectile =
  let newPos = projPos projectile `vAdd` (dt `vScale` projVelocity projectile)
      newLifetime = projLifetime projectile - dt
  in projectile { projPos = newPos, projLifetime = newLifetime }

cleanupProjectiles :: World -> World
cleanupProjectiles world =
  let active = Map.filter (\p -> projLifetime p > 0) (gsProjectiles world)
  in world { gsProjectiles = active }