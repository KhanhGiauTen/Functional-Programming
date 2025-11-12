{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerApp (
    initServerState,
    gameLoop,
    application
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as XSTM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import Control.Exception (finally, catch)
import Control.Monad (foldM, forever, forM_)
import System.Random (StdGen, mkStdGen, randomIO)
import Data.Foldable (foldl')

-- Import từ 'shared'
import Types.Common (EntityID, EntityState(..), Direction(..), Vector2D(..), vAdd, vScale, vSub, vMag, normalizeVec, isZeroVec)
import Types.GameTypes (GameState(..))
import Types.Player (Player(..), PlayerMage(..)) 
import Types.NetworkTypes (SpellCastType(..), PlayerInput(..))
import Types.Account (LoginResponse(..))
import Network.Protocol (ClientMessage(..), ServerMessage(..))
import Types.Enemy (Enemy(..), AIState(..))
import Types.Config
  ( enemyAttackCooldown
  , enemyAttackDamage
  , enemyAttackRange
  , enemyDeathDuration
  , enemyHitDuration
  , enemyMoveSpeed
  , mageAttackRadius
  , mageAttackDamage
  , playerRespawnDelay
  )

-- Import từ 'server'
import Systems.EntitySystem (World, addPlayer, getPlayer, movePlayer, resetPlayerState, respawnPlayer, spawnPoint)
import qualified Systems.EntitySystem as Systems
import qualified Systems.SkillSystem as SkillSystem
import qualified Systems.PhysicsSystem as PhysicsSystem
import qualified Systems.DungeonSystem as DungeonSystem
import qualified Systems.CombatSystem as CombatSystem


-- | ServerState chứa toàn bộ trạng thái server
data ServerState = ServerState
  { ssWorld      :: XSTM.TVar World
  , ssClients    :: XSTM.TVar (Map.Map EntityID WS.Connection)
  , ssNextID     :: XSTM.TVar EntityID
  , ssInputQueue :: XSTM.TQueue (EntityID, PlayerInput)
  , ssRng        :: XSTM.TVar StdGen
  , ssEnemyCooldowns :: XSTM.TVar (Map.Map EntityID Float)
  , ssEnemyDeathTimers :: XSTM.TVar (Map.Map EntityID Float)
  , ssSpawnTimer :: XSTM.TVar Float
  }

-- | Hàm khởi tạo State
initServerState :: IO ServerState
initServerState = do
  putStrLn "[ServerApp] Initializing game world..."

  seed <- randomIO :: IO Int
  rngTVar <- XSTM.newTVarIO (mkStdGen seed)
  worldTVar <- XSTM.newTVarIO Systems.initialWorld

  clientsTVar <- XSTM.newTVarIO Map.empty
  nextIdTVar <- XSTM.newTVarIO 1
  inputQueue <- XSTM.newTQueueIO
  cooldownVar <- XSTM.newTVarIO Map.empty
  deathVar <- XSTM.newTVarIO Map.empty
  spawnTimerVar <- XSTM.newTVarIO 2.0
  return ServerState
    { ssWorld = worldTVar
    , ssClients = clientsTVar
    , ssNextID = nextIdTVar
    , ssInputQueue = inputQueue
    , ssRng = rngTVar
    , ssEnemyCooldowns = cooldownVar
    , ssEnemyDeathTimers = deathVar
    , ssSpawnTimer = spawnTimerVar
    }

drainTQueue :: XSTM.TQueue a -> XSTM.STM [a]
drainTQueue queue = do
  mVal <- XSTM.tryReadTQueue queue
  case mVal of
    Nothing  -> pure []
    Just val -> (val :) <$> drainTQueue queue

gameLoop :: ServerState -> IO ()
gameLoop serverState = forever $ do
  let dt         = 1.0 / 60.0
      worldTVar  = ssWorld serverState
      inputQueue = ssInputQueue serverState

  inputs <- XSTM.atomically $ drainTQueue inputQueue
  let inputPlayerIDs = Map.fromList $ fmap (\(pid, _) -> (pid, True)) inputs

  XSTM.atomically $ do
    world0 <- XSTM.readTVar worldTVar
    world1 <- processInputs serverState world0 inputs

    let worldIdleReset   = updatePlayerTimers dt inputPlayerIDs world1
        worldAfterPhys   = PhysicsSystem.tick dt worldIdleReset
        worldAfterCombat = CombatSystem.tick worldAfterPhys

    worldFinal <- advanceWorldState dt serverState worldAfterCombat
    XSTM.writeTVar worldTVar worldFinal

  broadcastGameState serverState
  threadDelay 16666

updatePlayerTimers :: Float -> Map.Map EntityID Bool -> World -> World
updatePlayerTimers dt inputPlayerIDs world =
  let
    tickTimers player =
      let playerIdleReset =
            if Map.member (playerId player) inputPlayerIDs
              then player
              else resetPlayerState player

          (newState, newTimer) =
            if playerAttackTimer playerIdleReset > 0
              then
                let t = playerAttackTimer playerIdleReset - dt
                in if t <= 0
                     then (Idle, 0)
                     else (Attacking, t)
              else (playerState playerIdleReset, 0)

      in playerIdleReset { playerState = newState, playerAttackTimer = newTimer }

  in world { gsPlayers = Map.map tickTimers (gsPlayers world) }

processInputs :: ServerState -> World -> [(EntityID, PlayerInput)] -> XSTM.STM World
processInputs serverState = foldM (processInput serverState)
  where
    processInput :: ServerState -> World -> (EntityID, PlayerInput) -> XSTM.STM World
    processInput _ world (pId, InputMove dir) =
      pure (movePlayer pId dir world)

    processInput _ world (pId, InputCastSpell (CastBasicAttack _dir)) =
      case getPlayer pId world of
        Nothing -> pure world
        Just player ->
          if playerState player == Idle || playerState player == Walking
            then do
              let worldAoE = applyMeleeAoE player world
              pure (setPlayerAttackingState pId worldAoE)
            else pure world

    processInput _ world (_, InputCastSpell _) = pure world
    processInput _ world (_, InputPickupItems) = pure world

    setPlayerAttackingState :: EntityID -> World -> World
    setPlayerAttackingState pid worldWithProj =
      case getPlayer pid worldWithProj of
        Nothing -> worldWithProj
        Just playerToUpdate ->
          let updatedPlayer = playerToUpdate
                { playerState = Attacking
                , playerAttackTimer = 0.5
                }
          in worldWithProj { gsPlayers = Map.insert pid updatedPlayer (gsPlayers worldWithProj) }

    -- Area-of-effect melee hit around the mage (no projectile assets)
    applyMeleeAoE :: Player -> World -> World
    applyMeleeAoE p w =
      let origin = playerPos p
          enemies' = Map.map (hitIfInRange origin) (gsEnemies w)
      in w { gsEnemies = enemies' }
      where
        hitIfInRange origin e =
          let d = vMag (enemyPos e `vSub` origin)
          in if d <= mageAttackRadius
               then
                 let newHealth = max 0 (enemyHealth e - mageAttackDamage)
                     newState  = if newHealth <= 0 then Dead else Hit
                 in e { enemyHealth = newHealth
                      , enemyState  = newState
                      , enemyAIState = if newHealth <= 0 then AI_Idle else enemyAIState e
                      , enemyTarget  = if newHealth <= 0 then Nothing else enemyTarget e
                      }
               else e

advanceWorldState :: Float -> ServerState -> World -> XSTM.STM World
advanceWorldState dt serverState world = do
  cooldowns <- XSTM.readTVar (ssEnemyCooldowns serverState)
  deathTimers <- XSTM.readTVar (ssEnemyDeathTimers serverState)
  spawnTimer <- XSTM.readTVar (ssSpawnTimer serverState)
  rng <- XSTM.readTVar (ssRng serverState)

  let (worldAI, cooldowns', deathTimers', damageMap) = updateEnemyAI dt world cooldowns deathTimers
      (worldDamaged, newlyDeadPlayers) = applyEnemyDamage damageMap worldAI
      worldScheduled = scheduleRespawns newlyDeadPlayers worldDamaged
      worldRespawned = progressRespawns dt worldScheduled

      spawnTimer' = max 0 (spawnTimer - dt)
      desiredEnemyCount = 6
      currentEnemies = Map.size (gsEnemies worldRespawned)
      (worldSpawned, rng', spawnTimerFinal, cooldownsFinal, deathTimersFinal) =
        if currentEnemies < desiredEnemyCount && spawnTimer' <= 0
          then
            let beforeIds = Set.fromList (Map.keys (gsEnemies worldRespawned))
                (worldNew, rngNew) = DungeonSystem.spawnRandomEnemies 1 rng worldRespawned
                afterIds = Set.fromList (Map.keys (gsEnemies worldNew))
                newIds = Set.toList (Set.difference afterIds beforeIds)
                cooldownsWithNew = foldl' (\acc eid -> Map.insert eid 0 acc) cooldowns' newIds
            in (worldNew, rngNew, 2.5, cooldownsWithNew, deathTimers')
          else (worldRespawned, rng, spawnTimer', cooldowns', deathTimers')

  XSTM.writeTVar (ssEnemyCooldowns serverState) cooldownsFinal
  XSTM.writeTVar (ssEnemyDeathTimers serverState) deathTimersFinal
  XSTM.writeTVar (ssSpawnTimer serverState) spawnTimerFinal
  XSTM.writeTVar (ssRng serverState) rng'
  pure worldSpawned

type DamageMap = Map.Map EntityID Int

updateEnemyAI
  :: Float
  -> World
  -> Map.Map EntityID Float
  -> Map.Map EntityID Float
  -> (World, Map.Map EntityID Float, Map.Map EntityID Float, DamageMap)
updateEnemyAI dt world cooldowns deathTimers =
  let players = gsPlayers world
      alivePlayers = Map.filter (\p -> playerHealth p > 0) players
      (enemyMap, cooldowns', deathTimers', damageMap) =
        Map.foldlWithKey' (stepEnemy alivePlayers) (Map.empty, Map.empty, Map.empty, Map.empty) (gsEnemies world)
  in (world { gsEnemies = enemyMap }, cooldowns', deathTimers', damageMap)
  where
    stepEnemy alivePlayers (enemyAcc, cooldownAcc, deathAcc, damageAcc) eId enemy
      | enemyHealth enemy <= 0 =
          let total0 = Map.findWithDefault (enemyHitDuration + enemyDeathDuration) eId deathTimers
              total  = max 0 (total0 - dt)
          in if total <= 0
               then (enemyAcc, cooldownAcc, deathAcc, damageAcc)
               else
                 let phaseEnemy =
                       if total > enemyDeathDuration
                         then enemy { enemyHealth = 0, enemyState = Hit,  enemyAIState = AI_Idle, enemyTarget = Nothing }
                         else enemy { enemyHealth = 0, enemyState = Dead, enemyAIState = AI_Idle, enemyTarget = Nothing }
                 in ( Map.insert eId phaseEnemy enemyAcc
                    , cooldownAcc
                    , Map.insert eId total deathAcc
                    , damageAcc
                    )
      | Map.null alivePlayers =
          let idleEnemy = enemy
                { enemyState = Idle
                , enemyAIState = AI_Idle
                , enemyTarget = Nothing
                }
              cooldown = max 0 (Map.findWithDefault 0 eId cooldowns - dt)
          in ( Map.insert eId idleEnemy enemyAcc
             , Map.insert eId cooldown cooldownAcc
             , Map.delete eId deathAcc
             , damageAcc
             )
      | otherwise =
          case closestPlayer alivePlayers (enemyPos enemy) of
            Nothing ->
              let idleEnemy = enemy
                    { enemyState = Idle
                    , enemyAIState = AI_Idle
                    , enemyTarget = Nothing
                    }
                  cooldown = max 0 (Map.findWithDefault 0 eId cooldowns - dt)
              in ( Map.insert eId idleEnemy enemyAcc
                 , Map.insert eId cooldown cooldownAcc
                 , Map.delete eId deathAcc
                 , damageAcc
                 )
            Just (pid, targetPlayer, offsetVec, dist) ->
              let dir = directionFromVec offsetVec (enemyDir enemy)
                  cooldown0 = max 0 (Map.findWithDefault 0 eId cooldowns - dt)
              in if dist <= enemyAttackRange
                   then
                     let (cooldown', damageAcc') =
                           if cooldown0 <= 0
                             then (enemyAttackCooldown, Map.insertWith (+) pid enemyAttackDamage damageAcc)
                             else (cooldown0, damageAcc)
                         attackingEnemy = enemy
                           { enemyState = Attacking
                           , enemyDir = dir
                           , enemyAIState = AI_Attacking pid
                           , enemyTarget = Just pid
                           }
                     in ( Map.insert eId attackingEnemy enemyAcc
                        , Map.insert eId cooldown' cooldownAcc
                        , Map.delete eId deathAcc
                        , damageAcc'
                        )
                   else
                     let moveDir = if isZeroVec offsetVec then Vec 0 0 else normalizeVec offsetVec
                         moveVec = vScale (enemyMoveSpeed * dt) moveDir
                         newPos = enemyPos enemy `vAdd` moveVec
                         chasingEnemy = enemy
                           { enemyPos = newPos
                           , enemyDir = dir
                           , enemyState = Walking
                           , enemyAIState = AI_Chasing pid
                           , enemyTarget = Just pid
                           }
                     in ( Map.insert eId chasingEnemy enemyAcc
                        , Map.insert eId cooldown0 cooldownAcc
                        , Map.delete eId deathAcc
                        , damageAcc
                        )

applyEnemyDamage :: DamageMap -> World -> (World, [EntityID])
applyEnemyDamage damageMap world =
  Map.foldlWithKey' apply (world, []) damageMap
  where
    apply (w, deaths) pid dmg =
      case Map.lookup pid (gsPlayers w) of
        Nothing -> (w, deaths)
        Just player ->
          let health' = max 0 (playerHealth player - dmg)
              wasAlive = playerHealth player > 0
              updatedPlayer = player
                { playerHealth = health'
                , playerState = if health' <= 0 then Dead else playerState player
                }
              players' = Map.insert pid updatedPlayer (gsPlayers w)
              deaths' = if wasAlive && health' <= 0 then pid : deaths else deaths
          in (w { gsPlayers = players' }, deaths')

scheduleRespawns :: [EntityID] -> World -> World
scheduleRespawns playerIds world =
  let (players', respawns') = foldl' step (gsPlayers world, gsRespawns world) playerIds
  in world { gsPlayers = players', gsRespawns = respawns' }
  where
    step (playersMap, respawnsMap) pid =
      case Map.lookup pid playersMap of
        Nothing -> (playersMap, respawnsMap)
        Just player ->
          let remainingLives = max 0 (playerLives player - 1)
              updatedPlayer = player
                { playerLives = remainingLives
                , playerHealth = 0
                , playerState = Dead
                }
          in if remainingLives >= 0 && playerLives player > 0
               then
                 ( Map.insert pid updatedPlayer playersMap
                 , Map.insert pid playerRespawnDelay respawnsMap
                 )
               else
                 ( Map.insert pid updatedPlayer playersMap
                 , Map.delete pid respawnsMap
                 )

progressRespawns :: Float -> World -> World
progressRespawns dt world =
  let respawnsTicked = Map.map (\t -> t - dt) (gsRespawns world)
      (ready, waiting) = Map.partition (<= 0) respawnsTicked
      playersRespawned = Map.foldlWithKey' (\acc pid _ -> respawnPlayer pid acc) (gsPlayers world) ready
  in world { gsPlayers = playersRespawned, gsRespawns = waiting }

closestPlayer :: Map.Map EntityID Player -> Vector2D -> Maybe (EntityID, Player, Vector2D, Float)
closestPlayer candidates origin =
  Map.foldlWithKey' step Nothing candidates
  where
    step Nothing pid player =
      let offset = playerPos player `vSub` origin
          dist = vMag offset
      in Just (pid, player, offset, dist)
    step (Just (bestPid, bestPlayer, bestOffset, bestDist)) pid player =
      let offset = playerPos player `vSub` origin
          dist = vMag offset
      in if dist < bestDist
           then Just (pid, player, offset, dist)
           else Just (bestPid, bestPlayer, bestOffset, bestDist)

directionFromVec :: Vector2D -> Direction -> Direction
directionFromVec (Vec x y) fallback
  | isZeroVec (Vec x y) = fallback
  | abs x >= abs y = if x >= 0 then DirRight else DirLeft
  | otherwise      = if y >= 0 then DirUp else DirDown

broadcastGameState :: ServerState -> IO ()
broadcastGameState serverState = do
  world   <- XSTM.readTVarIO (ssWorld serverState)
  clients <- XSTM.readTVarIO (ssClients serverState)

  let msg = Aeson.encode (SM_UpdateGameState $ Systems.worldToGameState world)
      sendSafe (pid, conn) =
        WS.sendTextData conn msg `catch` \(_ :: WS.ConnectionException) ->
          XSTM.atomically $ XSTM.modifyTVar' (ssClients serverState) (Map.delete pid)

  forM_ (Map.toList clients) sendSafe

application :: ServerState -> WS.ServerApp
application serverState pending = do
  conn <- WS.acceptRequest pending

  (pId, snapshot) <- XSTM.atomically $ do
    newId <- XSTM.readTVar (ssNextID serverState)
    XSTM.writeTVar (ssNextID serverState) (newId + 1)

    XSTM.modifyTVar' (ssClients serverState) (Map.insert newId conn)

    world0 <- XSTM.readTVar (ssWorld serverState)
    rng <- XSTM.readTVar (ssRng serverState)
    let (world1, _) = addPlayer newId world0
        desiredEnemyCount = 5
        missingEnemies = max 0 (desiredEnemyCount - Map.size (gsEnemies world1))
        (world2, rng') =
          if missingEnemies > 0
            then DungeonSystem.spawnRandomEnemies missingEnemies rng world1
            else (world1, rng)
    XSTM.writeTVar (ssWorld serverState) world2
    XSTM.writeTVar (ssRng serverState) rng'

    pure (newId, world2)

  putStrLn $ "[Server] Client " ++ show pId ++ " đã kết nối."

  WS.sendTextData conn (Aeson.encode (SM_AssignPlayerID pId))
  WS.sendTextData conn (Aeson.encode (SM_UpdateGameState $ Systems.worldToGameState snapshot))

  finally
    (clientHandler conn pId serverState)
    (disconnectHandler pId serverState)

clientHandler :: WS.Connection -> EntityID -> ServerState -> IO ()
clientHandler conn pId serverState =
  forever loop `catch` \(_ :: WS.ConnectionException) ->
    putStrLn $ "[Server] Connection closed for player " ++ show pId
  where
    loop = do
      msg <- WS.receiveData conn
      case Aeson.decode msg of
        Nothing -> putStrLn $ "[Server] Lỗi decode input từ client " ++ show pId
        Just (CM_Input playerInput) ->
          XSTM.atomically $ XSTM.writeTQueue (ssInputQueue serverState) (pId, playerInput)
        Just (CM_Login _) ->
          let response = LoginResponse True "Login OK"
          in WS.sendTextData conn (Aeson.encode (SM_LoginResponse response))
        Just (CM_SelectMage mageSel) ->
          XSTM.atomically $ do
            XSTM.modifyTVar' (ssWorld serverState) (\w ->
              case getPlayer pId w of
                Nothing -> w
                Just pl -> w { gsPlayers = Map.insert pId (pl { playerMage = mageSel }) (gsPlayers w) })

disconnectHandler :: EntityID -> ServerState -> IO ()
disconnectHandler pId serverState = do
  putStrLn $ "[Server] Client " ++ show pId ++ " đã ngắt kết nối."
  XSTM.atomically $ do
    XSTM.modifyTVar' (ssWorld serverState) (Systems.removePlayer pId)
    XSTM.modifyTVar' (ssClients serverState) (Map.delete pId)
