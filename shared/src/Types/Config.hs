module Types.Config where

-- | Core render surface dimensions.
screenWidth :: Float
screenWidth = 1280

screenHeight :: Float
screenHeight = 720

halfScreenWidth :: Float
halfScreenWidth = screenWidth / 2

halfScreenHeight :: Float
halfScreenHeight = screenHeight / 2

-- | Enemy behaviour tuning.
enemySpawnMargin :: Float
enemySpawnMargin = 60

enemySpawnGroundY :: Float
enemySpawnGroundY = -76

enemySpawnVerticalJitter :: Float
enemySpawnVerticalJitter = 48

enemyMoveSpeed :: Float
enemyMoveSpeed = 140

enemyAttackRange :: Float
enemyAttackRange = 80

enemyAttackDamage :: Int
-- Set to 5 so a Mage with 100 HP requires 20 enemy hits to die (100 / 5 = 20)
enemyAttackDamage = 5

enemyAttackCooldown :: Float
enemyAttackCooldown = 1.7

enemyDeathDuration :: Float
enemyDeathDuration = 0.8

-- | Short hit-stun window to show the 'hit' animation before death.
enemyHitDuration :: Float
enemyHitDuration = 0.2

-- | Player balancing.
playerDefaultLives :: Int
playerDefaultLives = 3

playerRespawnDelay :: Float
playerRespawnDelay = 4.0

-- | Sprite sizing for consistent rendering.
mageSpriteSize :: Int

-- | Mage basic attack as an area effect (no projectile assets).
-- Enemies within this radius from the mage are hit.
mageAttackRadius :: Float
mageAttackRadius = 160

-- | Damage dealt by the area hit. Set high to ensure kill on common mobs.
mageAttackDamage :: Int
mageAttackDamage = 9999
mageSpriteSize = 96

-- | Duration (seconds) of the basic attack animation; used client-side to time AoE damage application.
mageAttackDuration :: Float
mageAttackDuration = 0.4

enemySpriteSize :: Int
enemySpriteSize = 128

-- | Scale factor applied to mage Idle/Walk animations so they match Attack sprite look.
-- Attack frames often already appear visually smaller; we reduce non-attack states slightly.
mageNonAttackScale :: Float
mageNonAttackScale = 0.9

-- | Projectile movement tuning (mirrors server constants).
clientProjectileSpeed :: Float
clientProjectileSpeed = 500
