{-# LANGUAGE DeriveGeneric #-}

module Systems.EntitySystem
    ( World
    , initialWorld
    , addPlayer
    , removePlayer
    , getPlayer
    , movePlayer
    , resetPlayerState
    , worldToGameState
    , spawnPoint
    , playerId
    , playerAttackTimer
    , playerState
    , playerPos
    , playerDir
    , gsPlayers
    , gsEnemies
    , gsProjectiles
    , gsItems
    , gsRespawns
    , respawnPlayer
    ) where

import qualified Data.Map as Map
import Types.Common (EntityID, Vector2D(..), Direction(..), EntityState(..), vAdd, vScale, normalizeVec, isZeroVec)
import Types.Player (Player(..), PlayerMage(..))
import Types.GameTypes (GameState(..), initialGameState)
import Types.Config (playerDefaultLives)
import GHC.Generics (Generic)

-- World is just the GameState for now
type World = GameState

initialWorld :: World
initialWorld = initialGameState

-- --- Helper functions ---

-- Add a new player and return updated world and the created player
addPlayer :: EntityID -> World -> (World, Player)
addPlayer pId world =
    let spawnPos = spawnPoint pId
        newPlayer = Player
            { playerId = pId
            , playerPos = spawnPos
            , playerState = Idle
            , playerDir = DirDown
            , playerHealth = 100
            , maxHealth = 100
            , playerMana = 50
            , maxMana = 50
            , playerLives = playerDefaultLives
            , skill1Cooldown = 0
            , ultimateCooldown = 0
            , playerAttackTimer = 0
            , playerMage = if odd pId then Mage1 else Mage2
            }
        newPlayersMap = Map.insert pId newPlayer (gsPlayers world)
    in (world { gsPlayers = newPlayersMap }, newPlayer)

removePlayer :: EntityID -> World -> World
removePlayer pId world = world { gsPlayers = Map.delete pId (gsPlayers world) }

getPlayer :: EntityID -> World -> Maybe Player
getPlayer pId world = Map.lookup pId (gsPlayers world)

-- Move player by adding move vector (assumed to be delta already)
-- NÂNG CẤP: Di chuyển người chơi VÀ cập nhật State/Direction
movePlayer :: EntityID -> Vector2D -> World -> World
movePlayer pId moveDir world =
    case getPlayer pId world of
        Nothing -> world -- Người chơi không tồn tại
        Just player ->
            let normalizedDir = normalizeVec moveDir
            in if isZeroVec normalizedDir || playerState player == Dead
                then world
                else
                    let playerSpeed = 200.0
                        newPos = vAdd (playerPos player) (vScale playerSpeed normalizedDir)
                        newDir = if isZeroVec normalizedDir
                                    then playerDir player
                                    else vectorToDirection normalizedDir
                        updatedPlayer = player
                            { playerPos = newPos
                            , playerState = Walking
                            , playerDir = newDir
                            }
                        newPlayersMap = Map.insert pId updatedPlayer (gsPlayers world)
                    in world { gsPlayers = newPlayersMap }


-- NÂNG CẤP: Reset trạng thái của Player về Idle
resetPlayerState :: Player -> Player
resetPlayerState player =
    if playerState player == Walking
        then player { playerState = Idle }
        else player


-- (Helper: Chuyển Vector thành Direction)
vectorToDirection :: Vector2D -> Direction
vectorToDirection (Vec x y)
    | abs y > abs x = if y > 0 then DirUp else DirDown
    | otherwise     = if x > 0 then DirRight else DirLeft


-- | Simple alternating spawn points for up to four players (extend as needed)
spawnPoint :: EntityID -> Vector2D
spawnPoint pid =
    case abs pid `mod` 4 of
        0 -> Vec (-200) 0
        1 -> Vec 200 0
        2 -> Vec 0 200
        _ -> Vec 0 (-200)


queueRespawn :: EntityID -> Float -> World -> World
queueRespawn pid delay world =
    world { gsRespawns = Map.insert pid delay (gsRespawns world) }


processRespawns :: Float -> World -> World
processRespawns dt world =
    -- SỬA LỖI: Cần (\t -> t - dt)
    let ticked = Map.map (\t -> t - dt) (gsRespawns world)
        (ready, waiting) = Map.partition (<= 0) ticked
        -- SỬA LỖI: Cần (\tbl pid _ -> ...)
        respawnedPlayers = Map.foldlWithKey' (\tbl pid _ -> respawnPlayer pid tbl) (gsPlayers world) ready
    in world { gsPlayers = respawnedPlayers, gsRespawns = waiting }


respawnPlayer :: EntityID -> Map.Map EntityID Player -> Map.Map EntityID Player
respawnPlayer pid playersMap =
    case Map.lookup pid playersMap of
        Nothing -> playersMap
        Just player ->
            let spawnPos = spawnPoint pid
                respawned = player
                    { playerHealth = maxHealth player
                    , playerMana = maxMana player
                    , playerPos = spawnPos
                    , playerState = Idle
                    , playerDir = DirDown
                    , playerAttackTimer = 0
                    , playerMage = playerMage player
                    }
            in Map.insert pid respawned playersMap

-- Convert internal World to GameState for broadcasting (identity for now)
worldToGameState :: World -> GameState
worldToGameState = id

-- placeholders
addProjectile :: World -> World
addProjectile = id

updateAI :: Float -> World -> World
updateAI _ = id

updateCollisions :: World -> World
updateCollisions = id