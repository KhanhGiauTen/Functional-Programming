module Systems.CombatSystem
    ( tick
    ) where

import qualified Data.Map as Map
import Types.Common (EntityID, EntityState(..), Vector2D(..), vMag, vSub)
import Systems.EntitySystem (World) -- ĐÃ SỬA: Import World từ EntitySystem
import Types.GameTypes (GameState(..), Projectile(..)) -- Import GameState để truy cập fields
import Types.Enemy (Enemy(..), AIState(..))

-- | Hàm 'tick' cho hệ thống chiến đấu, chạy 60 lần/giây
tick :: World -> World
tick world =
    let 
        -- 1. Va chạm Đạn vs Kẻ thù
        (world1, projectilesToKeep) = checkProjectileEnemyCollision world
        -- 2. Dọn dẹp Projectiles đã va chạm
        world2 = world1 { gsProjectiles = projectilesToKeep }
    in world2

-- | Khoảng cách va chạm (Tạm thời: bán kính)
projectileRadius :: Float
projectileRadius = 5

enemyRadius :: Float
enemyRadius = 15

-- | Kiểm tra va chạm giữa tất cả đạn và kẻ thù
checkProjectileEnemyCollision :: World -> (World, Map.Map EntityID Projectile)
checkProjectileEnemyCollision world =
    let 
        -- Nâng cấp: Truy cập gsEnemies và gsProjectiles
        (updatedEnemies, collidedProjectiles) = 
            Map.foldlWithKey' checkProjAgainstAllEnemies (gsEnemies world, Map.empty) (gsProjectiles world)
        
        checkProjAgainstAllEnemies (enemiesMap, collidedSet) pId projectile =
            case Map.foldlWithKey' (checkCollision pId projectile) (enemiesMap, False) enemiesMap of
                (newEnemies, True) -> (newEnemies, Map.insert pId projectile collidedSet)
                (newEnemies, False) -> (newEnemies, collidedSet)
        
        checkCollision pId proj (enemiesMap, collided) eId enemy =
            let dist = vMag (projPos proj `vSub` enemyPos enemy)
                isColliding = dist < (projectileRadius + enemyRadius)
            in if isColliding
                 then
                   let damage = projDamage proj
                       newHealth = enemyHealth enemy - damage
                       enemyState'
                         | newHealth <= 0 = Dead
                         | otherwise      = Hit
                       updatedEnemy = enemy
                         { enemyHealth = newHealth
                         , enemyState = enemyState'
                         , enemyAIState = if newHealth <= 0 then AI_Idle else enemyAIState enemy
                         , enemyTarget = if newHealth <= 0 then Nothing else enemyTarget enemy
                         }
                   in (Map.insert eId updatedEnemy enemiesMap, True)
                 else (enemiesMap, collided)
               
        projectilesToKeep = Map.difference (gsProjectiles world) collidedProjectiles
    in 
        (world { gsEnemies = updatedEnemies }, projectilesToKeep)

-- No automatic cleanup; handled by enemy system after death animation