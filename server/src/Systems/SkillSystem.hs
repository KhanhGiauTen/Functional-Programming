module Systems.SkillSystem where

import qualified Data.Map as Map
import Types.Common (EntityID, Vector2D(..), normalizeVec, vScale)
import Types.GameTypes (GameState(..), Projectile(..))
import Types.Player (Player(..))

projectileLifetime :: Float
projectileLifetime = 2.0

projectileSpeed :: Float
projectileSpeed = 500.0

-- | Create a projectile for a basic attack. Returns the updated world.
castBasicAttack :: EntityID -> EntityID -> Player -> Vector2D -> GameState -> GameState
castBasicAttack ownerId newProjId player attackDir world =
  let dir = normalizeVec attackDir
  in if isZeroVec dir || playerHealth player <= 0
       then world
       else
         let startPos = playerPos player
             velocity = vScale projectileSpeed dir
             newProj = Projectile
               { projId = newProjId
               , projOwnerId = ownerId
               , projPos = startPos
               , projVelocity = velocity
               , projDamage = 20
               , projLifetime = projectileLifetime
               }
             newProjectiles = Map.insert newProjId newProj (gsProjectiles world)
         in world { gsProjectiles = newProjectiles }


isZeroVec :: Vector2D -> Bool
isZeroVec (Vec x y) = x == 0 && y == 0
