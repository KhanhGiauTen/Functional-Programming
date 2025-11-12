{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Word (Word32, Word8)
import Linear (V2(..), V4(..))
import System.Random (StdGen, mkStdGen, randomIO, randomR)
import qualified Network.WebSockets as WS
import qualified SDL
import qualified SDL.Font as SDL.Font
import qualified SDL.Image as SDL.Image
import qualified SDL.Raw as SDL.Raw
import qualified Data.Text as T
import Foreign.C.Types (CInt)

import Core.Animation (Animation(..), AnimationState(..), getAnimationName, updateAnimation)
import Core.Input (handleEvents)
import Core.Renderer (ClientWorld(..), enemyAnimationKey, renderGame)
import Core.PlayerAssets (animationKeyFor)
import Network.ClientNet (ClientNetEnv(..), networkLoop)
import Renderer.Resources (ResourceMap, emptyAnimation, loadAllResources)
import Systems.MapLoader (TileMap, loadTileMap)
import Network.Protocol (ClientMessage(..), ServerMessage(..))
import Types.Account (LoginRequest(..), LoginResponse(..))
import Types.Common
  ( Direction(..)
  , EntityID
  , EntityState(..)
  , Vector2D(..)
  , isZeroVec
  , normalizeVec
  , vAdd
  , vSub
  , vMag
  , vScale
  )
import Types.GameTypes (GameState(..), initialGameState)
import Types.NetworkTypes (PlayerInput(..), SpellCastType(..))
import Types.Player (Player(..), PlayerMage(..))
import qualified Types.Player as TP
import Types.Enemy (AIState(..), Enemy(..), EnemyType(..))
import Types.Config
  ( enemyAttackCooldown
  , enemyAttackDamage
  , enemyAttackRange
  , enemyDeathDuration
  , enemyHitDuration
  , enemyMoveSpeed
  , enemySpawnGroundY
  , enemySpawnMargin
  , enemySpawnVerticalJitter
  , halfScreenHeight
  , halfScreenWidth
  , mageSpriteSize
  , mageAttackRadius
  , mageAttackDamage
  , mageAttackDuration
  , playerDefaultLives
  , playerRespawnDelay
  , screenHeight
  , screenWidth
  )
import Text.Printf (printf)
import UI.Menu
import qualified UI.Menu as Menu
  ( MenuEvent(..)
  , MenuInput(..)
  , MenuState(..)
  , ActiveField(..)
  , LoginState(..)
  , MainMenuState(..)
  , CharacterSelectState(..)
  , LobbyState(..)
  , LobbyPlayer(..)
  , LoginHover(..)
  , MainHover(..)
  , CharHover(..)
  , LobbyHover(..)
  , MageOption(..)
  , Rect(..)
  , initialCharacterSelect
  , initialLobbyState
  , initialMainMenu
  , initialMenuState
  , handleMenuInput
  , loginLoginButtonRect
  , loginPanelRect
  , loginPasswordRect
  , loginRegisterButtonRect
  , loginUsernameRect
  , lobbyBackRect
  , lobbyPanelRect
  , lobbyReadyRect
  , lobbyStartRect
  , mageCardRect
  , mageSlots
  , mainLogoutRect
  , mainPanelRect
  , mainPlayRect
  , mainSelectRect
  , pointInsideRect
  , mageLabel
  , setCharacterStatus
  , setLobbyStatus
  , setLoginStatus
  , setMainMessage
  , updateMenuState
  , characterPanelRect
  , characterConfirmRect
  , characterBackRect
  )

data AppWorld = AppWorld
  { awGame    :: TVar GameState
  , awAnims   :: TVar (Map.Map EntityID AnimationState)
  , awEnemyAnims :: TVar (Map.Map EntityID AnimationState)
  , awRes     :: ResourceMap
  , awTileMap :: Maybe TileMap
  , awLocalPlayerId :: EntityID
  , awRandom :: TVar StdGen
  , awSpawnTimer :: TVar Float
  , awNextEnemyId :: TVar EntityID
  , awEnemyTimers :: TVar (Map.Map EntityID Float)      -- attack cooldown timers per enemy
  , awDeathTimers :: TVar (Map.Map EntityID Float)      -- time until enemy is removed after death
  , awInput   :: TQueue PlayerInput
  , awConn    :: TVar (Maybe WS.Connection)
  , awMyId    :: TVar (Maybe EntityID)
  , awRunning :: TVar Bool
  , awMenu    :: TVar (Maybe MenuState)
  , awServerMessages :: TQueue ServerMessage
  , awFont    :: SDL.Font.Font
  , awPlayerName :: TVar (Maybe String)
  , awSelectedMage :: TVar (Maybe MageOption)
  }

screenWidthInt, screenHeightInt :: Int
screenWidthInt = round screenWidth
screenHeightInt = round screenHeight

screenWidthC, screenHeightC :: CInt
screenWidthC = fromIntegral screenWidthInt
screenHeightC = fromIntegral screenHeightInt

defaultAccounts :: Map.Map String String
defaultAccounts = Map.fromList
  [ ("admin1", "123")
  , ("admin2", "123")
  ]

lookupDefaultAccount :: String -> Maybe String
lookupDefaultAccount name =
  let key = map toLower (trim name)
  in Map.lookup key defaultAccounts

main :: IO ()
main = do
  result <- try (runClientApp) :: IO (Either SomeException ())
  case result of
    Left err -> do
      let msg = "[Client] Fatal error: " ++ displayException err
      appendFile "client-debug.log" (msg ++ "\n")
      putStrLn msg
    Right _ -> pure ()

runClientApp :: IO ()
runClientApp = do
  SDL.initializeAll
  SDL.Image.initialize [SDL.Image.InitPNG]
  SDL.Font.initialize

  let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidthC screenHeightC }
      rendererConfig = SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  window <- SDL.createWindow "Arcane Duelists (SDL2)" windowConfig
  renderer <- SDL.createRenderer window (-1) rendererConfig

  putStrLn "Loading resources..."
  resources <- loadAllResources renderer
  font <- SDL.Font.load "C:\\Windows\\Fonts\\arial.ttf" 28

  mapResult <- loadTileMap "client/assets/maps/sample_map.json"
  tileMap <-
    case mapResult of
      Left err -> do
        let msg = "[Client] Failed to load tile map: " ++ err
        appendFile "client-debug.log" (msg ++ "\n")
        putStrLn msg
        pure Nothing
      Right tm -> pure (Just tm)

  seed <- randomIO :: IO Int
  let localPlayerId = 1
      baseState = initialGameState

  gameVar <- newTVarIO baseState
  animVar <- newTVarIO Map.empty
  enemyAnimVar <- newTVarIO Map.empty
  inputQueue <- newTQueueIO
  connVar <- newTVarIO Nothing
  myIdVar <- newTVarIO Nothing
  runningVar <- newTVarIO True
  randomVar <- newTVarIO (mkStdGen seed)
  spawnTimerVar <- newTVarIO 1.5
  nextEnemyVar <- newTVarIO 1000
  enemyTimersVar <- newTVarIO Map.empty
  deathTimersVar <- newTVarIO Map.empty
  menuVar <- newTVarIO (Just initialMenuState)
  messageQueue <- newTQueueIO
  playerNameVar <- newTVarIO Nothing
  selectedMageVar <- newTVarIO Nothing

  let app =
        AppWorld
          { awGame = gameVar
          , awAnims = animVar
          , awEnemyAnims = enemyAnimVar
          , awRes = resources
          , awTileMap = tileMap
          , awLocalPlayerId = localPlayerId
          , awRandom = randomVar
          , awSpawnTimer = spawnTimerVar
          , awNextEnemyId = nextEnemyVar
          , awEnemyTimers = enemyTimersVar
          , awDeathTimers = deathTimersVar
          , awInput = inputQueue
          , awConn = connVar
          , awMyId = myIdVar
          , awRunning = runningVar
          , awMenu = menuVar
          , awServerMessages = messageQueue
          , awFont = font
          , awPlayerName = playerNameVar
          , awSelectedMage = selectedMageVar
          }

  atomically $ ensurePlayerExistsSTM app

  putStrLn "Connecting to server..."
  let netEnv = ClientNetEnv
        { cneGame = gameVar
        , cneAnims = animVar
        , cneResources = resources
        , cneMyId = myIdVar
        , cneConn = connVar
        , cneRunning = runningVar
        , cneMessageQueue = messageQueue
        }
  _ <- forkIO $ networkLoop netEnv "127.0.0.1" 9160

  SDL.Raw.startTextInput
  clientGameLoop renderer app

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.Image.quit
  SDL.quit

clientGameLoop :: SDL.Renderer -> AppWorld -> IO ()
clientGameLoop renderer app = do
  running <- readTVarIO (awRunning app)
  when running $ do
    events <- SDL.pollEvents
    let quitRequested = any isCloseEvent events
    when quitRequested $ atomically $ writeTVar (awRunning app) False

    stillRunning <- readTVarIO (awRunning app)
    when stillRunning $ do
      menuOpt <- readTVarIO (awMenu app)
      case menuOpt of
        Just _  -> SDL.Raw.startTextInput
        Nothing -> SDL.stopTextInput

      case menuOpt of
        Just _ -> do
          processMenuEvents app events
          processLoginResponses app
          tickMenuState app
        Nothing -> do
          handleEvents events (awInput app) (awRunning app)
          updateClient deltaTime app
          sendInputs app

      SDL.rendererDrawColor renderer SDL.$= V4 10 10 25 255
      SDL.clear renderer
      menuAfter <- readTVarIO (awMenu app)
      case menuAfter of
        Just menuState -> renderMenu renderer app menuState
        Nothing -> do
          gs <- readTVarIO (awGame app)
          playerAnims <- readTVarIO (awAnims app)
          enemyAnims <- readTVarIO (awEnemyAnims app)
          myId <- readTVarIO (awMyId app)
          selMage <- readTVarIO (awSelectedMage app)
          let selPrefix = fmap (\m -> case m of { Menu.Mage1 -> "mage1"; Menu.Mage2 -> "mage2" }) selMage  -- MageOption (menu)
          renderGame renderer (awRes app) (ClientWorld gs playerAnims enemyAnims (awTileMap app) selPrefix) myId
          renderGameplayOverlay renderer (awFont app) gs (awLocalPlayerId app) myId
      SDL.present renderer

      SDL.delay frameDelayMs
      clientGameLoop renderer app
  where
    frameDelayMs :: Word32
    frameDelayMs = 16
    deltaTime :: Float
    deltaTime = 1 / 60

    isCloseEvent :: SDL.Event -> Bool
    isCloseEvent ev =
      case SDL.eventPayload ev of
        SDL.QuitEvent -> True
        _             -> False

    tickMenuState :: AppWorld -> IO ()
    tickMenuState world =
      atomically $ modifyTVar' (awMenu world) (fmap (updateMenuState deltaTime))

renderGameplayOverlay :: SDL.Renderer -> SDL.Font.Font -> GameState -> EntityID -> Maybe EntityID -> IO ()
renderGameplayOverlay renderer font gs fallbackPid mMyId =
  case Map.lookup targetPid (gsPlayers gs) of
    Nothing -> pure ()
    Just player ->
      if playerHealth player > 0
        then pure ()
        else do
          let lives = playerLives player
              respawnTimer = Map.findWithDefault playerRespawnDelay targetPid (gsRespawns gs)
              countdown = max 0 respawnTimer
          if lives <= 0
            then do
              drawCenterLine (-20) (rgba 250 220 220 255) "You have been defeated"
              drawCenterLine 30 (rgba 220 220 255 230) "Press Esc to return to menu"
            else do
              drawCenterLine (-40) (rgba 250 230 230 255) "You fell in battle"
              drawCenterLine 10 (rgba 220 220 255 230) (printf "Respawning in %.1fs" countdown)
              drawCenterLine 54 (rgba 220 220 255 230) (printf "Lives remaining: %d" lives)
  where
    targetPid = fromMaybe fallbackPid mMyId
    centerY = screenHeightInt `div` 2

    drawCenterLine :: Int -> SDL.V4 Word8 -> String -> IO ()
    drawCenterLine offset color label =
      withTextTexture renderer font color label $ \tex (w, h) -> do
        let x = (screenWidthInt - w) `div` 2
            y = centerY + offset - h `div` 2
            dest = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))
        SDL.copy renderer tex Nothing (Just dest)

processMenuEvents :: AppWorld -> [SDL.Event] -> IO ()
processMenuEvents app events = forM_ events handleEvent
  where
    handleEvent ev =
      case SDL.eventPayload ev of
        SDL.MouseMotionEvent motion ->
          let (mx, my) = screenToUi (SDL.mouseMotionEventPos motion)
          in sendMenuInput (InputMouseMove mx my)
        SDL.MouseButtonEvent btn
          | SDL.mouseButtonEventMotion btn == SDL.Pressed
          , SDL.mouseButtonEventButton btn == SDL.ButtonLeft ->
              let (mx, my) = screenToUi (SDL.mouseButtonEventPos btn)
              in sendMenuInput (InputMouseClick mx my)
          | otherwise -> pure ()
        SDL.TextInputEvent txtEvent ->
          let txt = T.unpack (SDL.textInputEventText txtEvent)
          in unless (null txt) $ sendMenuInput (InputText txt)
        SDL.KeyboardEvent keyEvent
          | SDL.keyboardEventKeyMotion keyEvent == SDL.Pressed ->
              case SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent) of
                SDL.KeycodeBackspace -> sendMenuInput InputBackspace
                SDL.KeycodeTab       -> sendMenuInput InputTab
                SDL.KeycodeReturn    -> sendMenuInput InputEnter
                SDL.KeycodeEscape    -> sendMenuInput InputEscape
                _                    -> pure ()
          | otherwise -> pure ()
        _ -> pure ()

    sendMenuInput input = do
      evt <- atomically $ do
        menuOpt <- readTVar (awMenu app)
        case menuOpt of
          Nothing -> pure MenuNoEvent
          Just menuState ->
            let (menu', menuEvt) = handleMenuInput input menuState
            in writeTVar (awMenu app) (Just menu') >> pure menuEvt
      performMenuEvent app evt
    screenToUi (SDL.P (V2 x y)) = (fromIntegral x - halfScreenWidth, halfScreenHeight - fromIntegral y)

processLoginResponses :: AppWorld -> IO ()
processLoginResponses app = go
  where
    go = do
      mMsg <- atomically $ tryReadTQueue (awServerMessages app)
      case mMsg of
        Nothing -> pure ()
        Just (SM_LoginResponse (LoginResponse ok msgText)) -> do
          menuSnapshot <- readTVarIO (awMenu app)
          storedName <- readTVarIO (awPlayerName app)
          let fallbackName =
                case menuSnapshot of
                  Just (MenuLogin st) -> nonEmptyOr "Player" (loginUsername st)
                  Just (MenuMain st)  -> nonEmptyOr "Player" (mainUsername st)
                  _ -> fromMaybe "Player" storedName
              message = T.unpack msgText
          if ok
            then atomically $ do
              writeTVar (awPlayerName app) (Just fallbackName)
              writeTVar (awMenu app) (Just (initialMainMenu fallbackName))
            else atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just message)))
          go
        Just _ -> go

    nonEmptyOr :: String -> String -> String
    nonEmptyOr def txt
      | null txt  = def
      | otherwise = txt

renderMenu :: SDL.Renderer -> AppWorld -> MenuState -> IO ()
renderMenu renderer app state =
  case state of
    MenuLogin st      -> renderLoginScreen renderer (awFont app) st
    MenuMain st       -> renderMainMenuScreen renderer (awFont app) st
    MenuCharacter st  -> renderCharacterScreen renderer (awFont app) (awRes app) st
    MenuLobby st      -> renderLobbyScreen renderer (awFont app) st

performMenuEvent :: AppWorld -> MenuEvent -> IO ()
performMenuEvent _ MenuNoEvent = pure ()
performMenuEvent app (MenuAttemptLogin user pass)
  | null (trim user) || null (trim pass) =
      atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just "Please enter username and password.")))
  | otherwise =
      case lookupDefaultAccount user of
        Just expectedPassword
          | pass == expectedPassword -> do
              let normalizedName = trim user
              atomically $ do
                writeTVar (awPlayerName app) (Just normalizedName)
                writeTVar (awMenu app) (Just (initialMainMenu normalizedName))
        Just _ ->
          atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just "Invalid username or password.")))
        Nothing -> do
          atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just "Authenticating...")))
          mConn <- readTVarIO (awConn app)
          case mConn of
            Nothing -> atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just "Not connected to server.")))
            Just conn -> do
              let req = LoginRequest (T.pack user) (T.pack pass)
              WS.sendTextData conn (Aeson.encode (CM_Login req))
performMenuEvent app (MenuAttemptRegister _ _) =
  atomically $ modifyTVar' (awMenu app) (fmap (setLoginStatus (Just "Registration is not available.")))
performMenuEvent app MenuOpenCharacters = atomically $ do
  menuOpt <- readTVar (awMenu app)
  case menuOpt of
    Just (MenuMain _) -> writeTVar (awMenu app) (Just (initialCharacterSelect [Menu.Mage1, Menu.Mage2]))  -- MageOption list
    _ -> pure ()
performMenuEvent app MenuPlayQuick = do
  username <- currentUsername app
  atomically $ do
    writeTVar (awSelectedMage app) (Just Menu.Mage1)  -- MageOption
    writeTVar (awMenu app) (Just (initialLobbyState username Menu.Mage1))
performMenuEvent app MenuLogout = do
  atomically $ do
    writeTVar (awMenu app) (Just initialMenuState)
    writeTVar (awPlayerName app) Nothing
    writeTVar (awSelectedMage app) Nothing
performMenuEvent app (MenuSelectMage _) = pure ()
performMenuEvent app (MenuConfirmMage mage) = do
  username <- currentUsername app
  atomically $ do
    writeTVar (awSelectedMage app) (Just mage)
    writeTVar (awMenu app) (Just (initialLobbyState username mage))
  -- Send selection to server if connected
  mConn <- readTVarIO (awConn app)
  case mConn of
    Nothing -> pure ()
    Just conn -> do
      let sel = case mage of { Menu.Mage1 -> TP.Mage1; Menu.Mage2 -> TP.Mage2 }  -- convert MageOption -> PlayerMage
      WS.sendTextData conn (Aeson.encode (CM_SelectMage sel))
performMenuEvent app MenuBackToMain = do
  username <- currentUsername app
  atomically $ writeTVar (awMenu app) (Just (initialMainMenu username))
performMenuEvent app MenuStartGame = do
  menuSnapshot <- readTVarIO (awMenu app)
  case menuSnapshot of
    Just lobbyState@(MenuLobby st)
      | all lpReady (lobbyPlayers st) -> do
          atomically $ writeTVar (awMenu app) Nothing
          SDL.stopTextInput
      | otherwise ->
          atomically $ modifyTVar' (awMenu app) (const (Just (setLobbyStatus (Just "All players must be ready.") lobbyState)))
    _ -> pure ()
performMenuEvent app MenuToggleReady =
  atomically $ modifyTVar' (awMenu app) (fmap (setLobbyStatus Nothing))

currentUsername :: AppWorld -> IO String
currentUsername app = do
  stored <- readTVarIO (awPlayerName app)
  menuOpt <- readTVarIO (awMenu app)
  let fallback = fromMaybe "Player" stored
  pure $ case menuOpt of
    Just (MenuLogin st) -> nonEmpty fallback (loginUsername st)
    Just (MenuMain st)  -> nonEmpty fallback (mainUsername st)
    Just (MenuLobby _)  -> fallback
    Just (MenuCharacter _) -> fallback
    Nothing -> fallback

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

nonEmpty :: String -> String -> String
nonEmpty fallback candidate
  | null (trim candidate) = fallback
  | otherwise = candidate

renderLoginScreen :: SDL.Renderer -> SDL.Font.Font -> LoginState -> IO ()
renderLoginScreen renderer font st = do
  drawPanel renderer loginPanelRect
  drawTextCentered renderer font (rgba 255 255 255 255) (0, 200) "Arcane Duelists"
  drawTextCentered renderer font (rgba 210 210 235 255) (0, 150) "Sign in to continue"

  drawInputLabel renderer font "Username" (0, 110)
  drawInputField renderer font loginUsernameRect (loginUsername st) (loginActiveField st == FieldUsername) (loginHover st == Just HoverUsername) caretVisible False

  drawInputLabel renderer font "Password" (0, 10)
  drawInputField renderer font loginPasswordRect (loginPassword st) (loginActiveField st == FieldPassword) (loginHover st == Just HoverPassword) caretVisible True

  drawButton renderer font loginLoginButtonRect "Login" (loginHover st == Just HoverLoginButton) True
  drawButton renderer font loginRegisterButtonRect "Register" (loginHover st == Just HoverRegisterButton) True

  forM_ (loginStatus st) $ \msg -> drawTextCentered renderer font (rgba 230 110 110 255) (0, -240) msg
  where
    caretVisible = loginCaretPhase st < 0.5 || loginActiveField st == FieldUsername

renderMainMenuScreen :: SDL.Renderer -> SDL.Font.Font -> MainMenuState -> IO ()
renderMainMenuScreen renderer font st = do
  drawPanel renderer mainPanelRect
  drawTextCentered renderer font (rgba 255 255 255 255) (0, 190) ("Welcome, " ++ mainUsername st)
  drawTextCentered renderer font (rgba 200 200 230 255) (0, 150) "Choose an option"

  drawButton renderer font mainPlayRect "Quick Match" (mainHover st == Just HoverPlay) True
  drawButton renderer font mainSelectRect "Select Mage" (mainHover st == Just HoverCharacters) True
  drawButton renderer font mainLogoutRect "Logout" (mainHover st == Just HoverLogout) True

  forM_ (mainMessage st) $ \msg -> drawTextCentered renderer font (rgba 220 120 120 255) (0, -220) msg

renderCharacterScreen :: SDL.Renderer -> SDL.Font.Font -> ResourceMap -> CharacterSelectState -> IO ()
renderCharacterScreen renderer font resources st = do
  drawPanel renderer characterPanelRect
  drawTextCentered renderer font (rgba 255 255 255 255) (0, 230) "Select Your Mage"

  let positions = mageSlots (length (csOptions st))
  forM_ (zip (csOptions st) positions) $ \(mage, centerX) ->
    drawMageCard renderer font resources st mage centerX

  drawButton renderer font characterConfirmRect "Confirm" (csHover st == Just HoverConfirm) (isJust (csSelected st))
  drawButton renderer font characterBackRect "Back" (csHover st == Just HoverBack) True

  forM_ (csStatus st) $ \msg -> drawTextCentered renderer font (rgba 220 120 120 255) (0, -250) msg

renderLobbyScreen :: SDL.Renderer -> SDL.Font.Font -> LobbyState -> IO ()
renderLobbyScreen renderer font st = do
  drawPanel renderer lobbyPanelRect
  drawTextCentered renderer font (rgba 255 255 255 255) (0, 240) ("Room: " ++ lobbyRoomId st)

  let playersWithIndex = zip [0..] (lobbyPlayers st)
  forM_ playersWithIndex $ \(idx, player) ->
    let centerX = if idx == 0 then -200 else 200
    in drawLobbyPlayer renderer font centerX player

  let allReady = all lpReady (lobbyPlayers st)
  drawButton renderer font lobbyReadyRect "Toggle Ready" (lobbyHover st == Just HoverReady) True
  drawButton renderer font lobbyStartRect "Start Game" (lobbyHover st == Just HoverStart) allReady
  drawButton renderer font lobbyBackRect "Back" (lobbyHover st == Just HoverBackLobby) True

  let readyText = if allReady then "All players ready" else "Waiting for ready players"
  drawTextCentered renderer font (rgba 200 200 230 255) (0, -80) readyText
  forM_ (lobbyStatus st) $ \msg -> drawTextCentered renderer font (rgba 220 120 120 255) (0, -260) msg

drawMageCard :: SDL.Renderer -> SDL.Font.Font -> ResourceMap -> CharacterSelectState -> MageOption -> Float -> IO ()
drawMageCard renderer font resources st mage centerX = do
  let rect = mageCardRect centerX
      isSelected = csSelected st == Just mage
      isHovering = csHover st == Just (HoverMage mage)
      fillColor
        | isSelected = rgba 70 60 150 220
        | isHovering = rgba 60 60 110 200
        | otherwise  = rgba 40 40 90 170
      borderColor
        | isSelected = rgba 200 200 255 255
        | otherwise  = rgba 120 120 180 220

  fillRect renderer fillColor rect
  strokeRect renderer borderColor rect

  forM_ (magePreviewTexture resources mage) $ \tex ->
    drawTextureCentered renderer tex (centerX, 120) (150, 150)

  drawTextCentered renderer font (rgba 255 255 255 255) (centerX, -40) (mageLabel mage)

magePreviewTexture :: ResourceMap -> MageOption -> Maybe SDL.Texture
magePreviewTexture resources mage =
  let key = case mage of  -- MageOption here
        Menu.Mage1 -> "mage1_idle_down"
        Menu.Mage2 -> "mage2_idle_down"
  in Map.lookup key resources >>= listToMaybe . animFrames

drawTextureCentered :: SDL.Renderer -> SDL.Texture -> (Float, Float) -> (Float, Float) -> IO ()
drawTextureCentered renderer tex (cx, cy) (maxW, maxH) = do
  info <- SDL.queryTexture tex
  let texW = max 1.0 (realToFrac (SDL.textureWidth info))
      texH = max 1.0 (realToFrac (SDL.textureHeight info))
      scale = min (maxW / texW) (maxH / texH)
      destW = max 1 (round (texW * scale))
      destH = max 1 (round (texH * scale))
      (screenX, screenY) = uiToScreen (cx, cy)
      destRect = SDL.Rectangle (SDL.P (V2 (fromIntegral (screenX - destW `div` 2)) (fromIntegral (screenY - destH `div` 2)))) (V2 (fromIntegral destW) (fromIntegral destH))
  SDL.copy renderer tex Nothing (Just destRect)

drawLobbyPlayer :: SDL.Renderer -> SDL.Font.Font -> Float -> LobbyPlayer -> IO ()
drawLobbyPlayer renderer font centerX player = do
  let rect = Rect (centerX, 120) (220, 160)
      fillColor = if lpReady player then rgba 60 90 60 200 else rgba 70 70 90 200
  fillRect renderer fillColor rect
  strokeRect renderer (rgba 180 180 220 220) rect
  drawTextCentered renderer font (rgba 255 255 255 255) (centerX, 160) (lpName player)
  drawTextCentered renderer font (rgba 200 200 230 255) (centerX, 125) (maybe "Mage" mageLabel (lpMage player))
  drawTextCentered renderer font (if lpReady player then rgba 140 240 140 255 else rgba 240 160 120 255) (centerX, 90) (if lpReady player then "Ready" else "Not Ready")

drawInputLabel :: SDL.Renderer -> SDL.Font.Font -> String -> (Float, Float) -> IO ()
drawInputLabel renderer font label pos =
  drawTextCentered renderer font (rgba 200 200 230 255) pos label

drawInputField :: SDL.Renderer -> SDL.Font.Font -> Rect -> String -> Bool -> Bool -> Bool -> Bool -> IO ()
drawInputField renderer font rect value isActive isHover caretVisible isPassword = do
  let fillColor
        | isActive  = rgba 75 85 150 235
        | isHover   = rgba 62 70 135 225
        | otherwise = rgba 44 48 100 210
      borderColor
        | isActive  = rgba 210 210 255 255
        | otherwise = rgba 120 125 190 220
      displayText = if isPassword then replicate (length value) '*' else value

  fillRect renderer fillColor rect
  strokeRect renderer borderColor rect

  let SDL.Rectangle (SDL.P (V2 rxC ryC)) (V2 _ rhC) = rectToSDLRect rect
      rx = fromIntegral rxC :: Int
      ry = fromIntegral ryC :: Int
      rh = fromIntegral rhC :: Int
      textColor = rgba 240 240 255 255
      baselineX = rx + 18
      baselineY = ry + (rh `div` 2)

  if null displayText
    then when caretVisible $ drawCaret baselineX baselineY rh
    else withTextTexture renderer font textColor displayText $ \tex (tw, th) -> do
           let dest = SDL.Rectangle (SDL.P (V2 (fromIntegral baselineX) (fromIntegral (baselineY - th `div` 2)))) (V2 (fromIntegral tw) (fromIntegral th))
           SDL.copy renderer tex Nothing (Just dest)
           when caretVisible $ drawCaret (baselineX + tw + 2) baselineY rh

  where
    drawCaret x baselineY fieldHeight = do
      let caretRect = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral (baselineY - fieldHeight `div` 2 + 6)))) (V2 2 (fromIntegral (fieldHeight - 12)))
      SDL.rendererDrawColor renderer SDL.$= rgba 240 240 255 255
      SDL.fillRect renderer (Just caretRect)

drawButton :: SDL.Renderer -> SDL.Font.Font -> Rect -> String -> Bool -> Bool -> IO ()
drawButton renderer font rect label highlighted enabled = do
  let fillColor
        | not enabled = rgba 55 60 110 160
        | highlighted = rgba 100 110 200 240
        | otherwise   = rgba 70 75 155 220
      borderColor
        | highlighted = rgba 210 210 255 255
        | otherwise   = rgba 150 150 210 220
      textColor = if enabled then rgba 245 245 255 255 else rgba 170 170 210 200
  fillRect renderer fillColor rect
  strokeRect renderer borderColor rect
  drawTextCentered renderer font textColor (rectCenter rect) label

drawPanel :: SDL.Renderer -> Rect -> IO ()
drawPanel renderer rect = do
  fillRect renderer (rgba 0 0 0 90) (offsetRect rect 8 (-8))
  fillRect renderer (rgba 36 38 82 215) rect
  strokeRect renderer (rgba 170 170 230 210) rect

fillRect :: SDL.Renderer -> SDL.V4 Word8 -> Rect -> IO ()
fillRect renderer color rect = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just (rectToSDLRect rect))

strokeRect :: SDL.Renderer -> SDL.V4 Word8 -> Rect -> IO ()
strokeRect renderer color rect = do
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.drawRect renderer (Just (rectToSDLRect rect))

withTextTexture :: SDL.Renderer -> SDL.Font.Font -> SDL.V4 Word8 -> String -> (SDL.Texture -> (Int, Int) -> IO ()) -> IO ()
withTextTexture renderer font color label action = do
  surface <- SDL.Font.blended font color (T.pack label)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  info <- SDL.queryTexture texture
  let w = fromIntegral (SDL.textureWidth info)
      h = fromIntegral (SDL.textureHeight info)
  action texture (w, h)
  SDL.destroyTexture texture

drawTextCentered :: SDL.Renderer -> SDL.Font.Font -> SDL.V4 Word8 -> (Float, Float) -> String -> IO ()
drawTextCentered renderer font color (cx, cy) label =
  withTextTexture renderer font color label $ \tex (w, h) -> do
    let (sx, sy) = uiToScreen (cx, cy)
        dest = SDL.Rectangle (SDL.P (V2 (fromIntegral (sx - w `div` 2)) (fromIntegral (sy - h `div` 2)))) (V2 (fromIntegral w) (fromIntegral h))
    SDL.copy renderer tex Nothing (Just dest)

uiToScreen :: (Float, Float) -> (Int, Int)
uiToScreen (cx, cy) = (round (halfScreenWidth + cx), round (halfScreenHeight - cy))

offsetRect :: Rect -> Float -> Float -> Rect
offsetRect (Rect (cx, cy) size) dx dy = Rect (cx + dx, cy + dy) size

rectToSDLRect :: Rect -> SDL.Rectangle CInt
rectToSDLRect (Rect (cx, cy) (w, h)) =
  let x = round (halfScreenWidth + cx - w / 2)
      y = round (halfScreenHeight - cy - h / 2)
  in SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral (round w)) (fromIntegral (round h)))

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> SDL.V4 Word8
rgba r g b a = V4 r g b a

sendInputs :: AppWorld -> IO ()
sendInputs app = do
  inputs <- atomically $ drainQueue (awInput app)
  unless (null inputs) $ do
    applyLocalInputs app inputs
    mConn <- readTVarIO (awConn app)
    case mConn of
      Nothing   -> pure ()
      Just conn -> mapM_ (WS.sendTextData conn . Aeson.encode . CM_Input) inputs

drainQueue :: TQueue a -> STM [a]
drainQueue q = do
  mVal <- tryReadTQueue q
  case mVal of
    Nothing  -> pure []
    Just val -> (val :) <$> drainQueue q

applyLocalInputs :: AppWorld -> [PlayerInput] -> IO ()
applyLocalInputs app inputs = do
  mPid <- readTVarIO (awMyId app)
  let playerId = maybe (awLocalPlayerId app) id mPid
  atomically $ modifyTVar' (awGame app) (applyInputsToGame playerId inputs)

applyInputsToGame :: EntityID -> [PlayerInput] -> GameState -> GameState
applyInputsToGame playerId inputs gs =
  gs { gsPlayers = foldl' step (gsPlayers gs) inputs }
  where
    step acc input = Map.alter (applyInput input) playerId acc

    applyInput :: PlayerInput -> Maybe Player -> Maybe Player
    applyInput input mPlayer = Just $ case (input, mPlayer) of
      (InputMove vec, Nothing) -> updateMovement vec (defaultPlayer playerId)
      (InputMove vec, Just p)  -> updateMovement vec p
      (InputCastSpell spell, Nothing) -> applySpell spell (defaultPlayer playerId)
      (InputCastSpell spell, Just p)  -> applySpell spell p
      (InputPickupItems, Nothing)     -> defaultPlayer playerId
      (InputPickupItems, Just p)      -> p

    updateMovement :: Vector2D -> Player -> Player
    updateMovement vec player
      | isZeroVec vec = player { playerState = Idle }
      | otherwise     =
          let dir = directionFromVec vec (playerDir player)
              stepVec = vScale moveStep (normalizeVec vec)
              newPos = playerPos player `vAdd` stepVec
          in player
               { playerPos = newPos
               , playerState = Walking
               , playerDir = dir
               }

    applySpell :: SpellCastType -> Player -> Player
    applySpell spell player =
      let dirUpdate = case spell of
            CastBasicAttack vec -> directionFromVec vec (playerDir player)
            _                   -> playerDir player
      in player
           { playerState = Attacking
           , playerDir = dirUpdate
           , playerAttackTimer = mageAttackDuration
           }

    moveStep :: Float
    moveStep = 4

    -- attack duration now comes from config (mageAttackDuration)

directionFromVec :: Vector2D -> Direction -> Direction
directionFromVec vec fallback
  | isZeroVec vec = fallback
  | absX >= absY = if x >= 0 then DirRight else DirLeft
  | otherwise    = if y >= 0 then DirUp else DirDown
  where
    Vec x y = vec
    absX = abs x
    absY = abs y

maxEnemyCount :: Int
maxEnemyCount = 6

spawnCooldownSeconds :: Float
spawnCooldownSeconds = 3.0

spawnChance :: Float
spawnChance = 0.55


updateClient :: Float -> AppWorld -> IO ()
updateClient dt app = do
  let res = awRes app
  mPid <- readTVarIO (awMyId app)
  let activeId = maybe (awLocalPlayerId app) id mPid
  connActive <- isJust <$> readTVarIO (awConn app)
  let useLocalSim = not connActive
  (gsPrepared, timersPrepared, dTimersPrepared, mySel) <- atomically $ do
    ensurePlayerExistsSTM app
    when useLocalSim $ spawnEnemiesSTM dt app
    gs <- readTVar (awGame app)
    timers <- readTVar (awEnemyTimers app)
    dTimers <- readTVar (awDeathTimers app)
    selMage <- readTVar (awSelectedMage app)
    let mySelLocal = fmap (\m -> (activeId, case m of { Menu.Mage1 -> "mage1"; Menu.Mage2 -> "mage2" })) selMage  -- MageOption
        (gsp, tP, dP) = if useLocalSim
                          then progressLocalSimulation dt activeId timers dTimers gs
                          else (gs, timers, dTimers)
    when useLocalSim $ do
      writeTVar (awGame app) gsp
      writeTVar (awEnemyTimers app) tP
      writeTVar (awDeathTimers app) dP
    pure (gsp, tP, dP, mySelLocal)

  atomically $ do
    playerAnims <- readTVar (awAnims app)
    let syncedPlayers = syncPlayerAnimations playerAnims res gsPrepared mySel
        updatedPlayers = Map.mapWithKey (stepPlayerAnimation dt res gsPrepared mySel) syncedPlayers
    writeTVar (awAnims app) updatedPlayers

    enemyAnims <- readTVar (awEnemyAnims app)
    let syncedEnemies = syncEnemyAnimations enemyAnims res gsPrepared
        updatedEnemies = Map.mapWithKey (stepEnemyAnimation dt res gsPrepared) syncedEnemies
    writeTVar (awEnemyAnims app) updatedEnemies

ensurePlayerExistsSTM :: AppWorld -> STM ()
ensurePlayerExistsSTM app = do
  mPid <- readTVar (awMyId app)
  let pid = maybe (awLocalPlayerId app) id mPid
  modifyTVar' (awGame app) $ \baseGs ->
    baseGs { gsPlayers = ensurePlayerExists pid (gsPlayers baseGs) }

spawnEnemiesSTM :: Float -> AppWorld -> STM ()
spawnEnemiesSTM dt app = do
  cooldown <- readTVar (awSpawnTimer app)
  let newCooldown = max 0 (cooldown - dt)
  writeTVar (awSpawnTimer app) newCooldown
  gs <- readTVar (awGame app)
  enemyTimers <- readTVar (awEnemyTimers app)
  gen <- readTVar (awRandom app)
  if newCooldown > 0 || Map.size (gsEnemies gs) >= maxEnemyCount
    then writeTVar (awRandom app) gen
    else do
      let (chance, gen1) = randomR (0.0 :: Float, 1.0 :: Float) gen
      if chance > spawnChance
        then writeTVar (awRandom app) gen1
        else do
          let (typeRoll, gen2) = randomR (0 :: Int, 1) gen1
              enemyType = if typeRoll == 0 then Golem else Elemental
              (sideRoll, gen3) = randomR (0 :: Int, 1) gen2
              spawnX = if sideRoll == 0
                         then negate (halfScreenWidth + enemySpawnMargin)
                         else halfScreenWidth + enemySpawnMargin
              (spawnJitter, gen4) = randomR (-enemySpawnVerticalJitter, enemySpawnVerticalJitter) gen3
              spawnDir = if sideRoll == 0 then DirRight else DirLeft
          nextEnemyId <- readTVar (awNextEnemyId app)
          let enemy = Enemy
                { enemyId = nextEnemyId
                , enemyType = enemyType
                , enemyPos = Vec spawnX (enemySpawnGroundY + spawnJitter)
                , enemyState = Walking
                , enemyDir = spawnDir
                , enemyHealth = 50
        , maxEnemyHealth = 50
        , enemyTarget = Nothing
                , enemyAIState = AI_Idle
                }
              newEnemies = Map.insert nextEnemyId enemy (gsEnemies gs)
              gs' = gs { gsEnemies = newEnemies }
          writeTVar (awRandom app) gen4
          writeTVar (awSpawnTimer app) spawnCooldownSeconds
          writeTVar (awNextEnemyId app) (nextEnemyId + 1)
          writeTVar (awEnemyTimers app) (Map.insert nextEnemyId 0 enemyTimers)
          writeTVar (awGame app) gs'


syncPlayerAnimations :: Map.Map EntityID AnimationState -> ResourceMap -> GameState -> Maybe (EntityID, String) -> Map.Map EntityID AnimationState
syncPlayerAnimations anims res gs mySel =
  Map.filterWithKey (\pid _ -> Map.member pid players) withAllPlayers
  where
    players = gsPlayers gs
    withAllPlayers = Map.foldlWithKey' ensure anims players
    ensure acc pid player = Map.alter (ensureState pid player) pid acc
    ensureState pid player Nothing  = Just (defaultPlayerAnimState res pid player mySel)
    ensureState _   _     (Just st) = Just st




stepPlayerAnimation :: Float -> ResourceMap -> GameState -> Maybe (EntityID, String) -> EntityID -> AnimationState -> AnimationState
stepPlayerAnimation dt res gs mySel pid animState =
  case Map.lookup pid (gsPlayers gs) of
    Nothing -> animState
    Just player ->
      let targetName = getAnimationName (playerState player) (playerDir player)
          targetKey = case mySel of
                        Just (mineId, pref) | pid == mineId ->
                          pref ++ suffixFromBase targetName
                        _ -> animationKeyFor player targetName
          desiredAnim = Map.findWithDefault emptyAnimation targetKey res
          currentState =
            if animName animState /= targetKey
              then AnimationState targetKey desiredAnim 0 0 False
              else animState { animCurrent = desiredAnim }
      in updateAnimation dt currentState

-- Helper: from "mage_idle_down" keep the part after "mage"
suffixFromBase :: String -> String
suffixFromBase base =
  let dropMage = drop 4 base
  in if take 4 base == "mage" then dropMage else '_' : base



defaultPlayerAnimState :: ResourceMap -> EntityID -> Player -> Maybe (EntityID, String) -> AnimationState
defaultPlayerAnimState res pid player mySel = AnimationState key anim 0 0 False
  where
    baseName = getAnimationName Idle (playerDir player)
    key = case mySel of
            Just (mineId, pref) | pid == mineId -> pref ++ suffixFromBase baseName
            _ -> animationKeyFor player baseName
    anim = Map.findWithDefault emptyAnimation key res


syncEnemyAnimations :: Map.Map EntityID AnimationState -> ResourceMap -> GameState -> Map.Map EntityID AnimationState
syncEnemyAnimations anims res gs =
  Map.filterWithKey (\eid _ -> Map.member eid enemies) withAllEnemies
  where
    enemies = gsEnemies gs
    withAllEnemies = Map.foldlWithKey' ensure anims enemies
    ensure acc eid enemy = Map.alter (ensureState enemy) eid acc
    ensureState enemy Nothing = Just (defaultEnemyAnimState res enemy)
    ensureState _     (Just st) = Just st

stepEnemyAnimation :: Float -> ResourceMap -> GameState -> EntityID -> AnimationState -> AnimationState
stepEnemyAnimation dt res gs eid animState =
  case Map.lookup eid (gsEnemies gs) of
    Nothing -> animState
    Just enemy ->
      let targetKey = enemyAnimationKey enemy
          desiredAnim = Map.findWithDefault emptyAnimation targetKey res
          currentState =
            if animName animState /= targetKey
              then AnimationState targetKey desiredAnim 0 0 False
              else animState { animCurrent = desiredAnim }
      in updateAnimation dt currentState

defaultEnemyAnimState :: ResourceMap -> Enemy -> AnimationState
defaultEnemyAnimState res enemy = AnimationState key anim 0 0 False
  where
    key = enemyAnimationKey enemy
    anim = Map.findWithDefault emptyAnimation key res


progressLocalSimulation :: Float -> EntityID -> Map.Map EntityID Float -> Map.Map EntityID Float -> GameState -> (GameState, Map.Map EntityID Float, Map.Map EntityID Float)
progressLocalSimulation dt localId timers deathTimers gs =
  let playersEnsured = ensurePlayerExists localId (gsPlayers gs)
      -- Apply AoE BEFORE ticking timers so the first frame of Attacking triggers damage
      enemiesAfterPlayer = applyPlayerAoE dt playersEnsured (gsEnemies gs)
      playersTicked = Map.map (tickPlayerTimer dt) playersEnsured
      accumulator0 = (playersTicked, timers)
      (accumulatorFinal, enemiesRaw) = Map.mapAccumWithKey (advanceEnemy dt) accumulator0 enemiesAfterPlayer
      (playersFinal, timersFinal) = accumulatorFinal
      -- Update death timers and decide removal/state
      (enemiesAfterDeath, deathTimers') = stepDeathTimers dt enemiesRaw deathTimers
      filteredTimers = Map.intersection timersFinal enemiesAfterDeath
      gs' = gs { gsPlayers = playersFinal, gsEnemies = enemiesAfterDeath }
  in (gs', filteredTimers, deathTimers')
  where
    advanceEnemy :: Float -> (Map.Map EntityID Player, Map.Map EntityID Float) -> EntityID -> Enemy -> ((Map.Map EntityID Player, Map.Map EntityID Float), Enemy)
    advanceEnemy delta (playerMap, timerMap) eid enemy
      | enemyHealth enemy <= 0 =
          let timers' = Map.delete eid timerMap
              enemy' = enemy { enemyState = Dead, enemyAIState = AI_Idle, enemyTarget = Nothing }
          in ((playerMap, timers'), enemy')
      | otherwise =
          let cooldown = Map.findWithDefault 0 eid timerMap
              cooldown' = max 0 (cooldown - delta)
              alivePlayers = Map.filter (\fp -> playerHealth fp > 0) playerMap
          in case closestTarget alivePlayers (enemyPos enemy) of
               Nothing ->
                 let timers' = Map.insert eid cooldown' timerMap
                     enemy' = enemy { enemyState = Idle, enemyAIState = AI_Idle, enemyTarget = Nothing }
                 in ((playerMap, timers'), enemy')
               Just (pid, targetPlayer, offsetVec, dist) ->
                 let facingDir = directionFromVec offsetVec (enemyDir enemy)
                     timersTicked = Map.insert eid cooldown' timerMap
                 in if dist <= enemyAttackRange
                      then
                        let (players', timers'') =
                              if cooldown' <= 0
                                then
                                  let newHealth = max 0 (playerHealth targetPlayer - enemyAttackDamage)
                                      updatedPlayer = targetPlayer { playerHealth = newHealth, playerState = Hit }
                                      adjustedPlayers = Map.insert pid updatedPlayer playerMap
                                  in (adjustedPlayers, Map.insert eid enemyAttackCooldown timersTicked)
                                else (playerMap, timersTicked)
                            enemy' = enemy
                              { enemyState = Attacking
                              , enemyDir = facingDir
                              , enemyAIState = AI_Attacking pid
                              , enemyTarget = Just pid
                              }
                        in ((players', timers''), enemy')
                      else
                        let moveDir = normalizeVec offsetVec
                            moveVec = vScale (enemyMoveSpeed * delta) moveDir
                            newPos = enemyPos enemy `vAdd` moveVec
                            enemy' = enemy
                              { enemyPos = newPos
                              , enemyDir = facingDir
                              , enemyState = Walking
                              , enemyAIState = AI_Chasing pid
                              , enemyTarget = Just pid
                              }
                        in ((playerMap, timersTicked), enemy')

    closestTarget :: Map.Map EntityID Player -> Vector2D -> Maybe (EntityID, Player, Vector2D, Float)
    closestTarget candidates originPos =
      Map.foldlWithKey' step Nothing candidates
      where
        step Nothing pid player =
          let offset = playerPos player `vSub` originPos
              dist = vMag offset
          in Just (pid, player, offset, dist)
        step (Just (bestPid, bestPlayer, bestOffset, bestDist)) pid player =
          let offset = playerPos player `vSub` originPos
              dist = vMag offset
          in if dist < bestDist
                then Just (pid, player, offset, dist)
                else Just (bestPid, bestPlayer, bestOffset, bestDist)
        step acc _ _ = acc

    tickPlayerTimer elapsed player =
      let newTimer = max 0 (playerAttackTimer player - elapsed)
          needsReset = newTimer <= 0 && playerState player == Attacking
          newState
            | playerHealth player <= 0 = Dead
            | needsReset = Idle
            | otherwise = playerState player
      in player { playerAttackTimer = newTimer, playerState = newState }

-- Keep enemies with 0 HP long enough to show hit then death animation
stepDeathTimers :: Float -> Map.Map EntityID Enemy -> Map.Map EntityID Float -> (Map.Map EntityID Enemy, Map.Map EntityID Float)
stepDeathTimers dt enemies deathTimers = (enemiesKept, timersKept)
  where
    -- Assign or tick timers
    timersTicked = Map.foldlWithKey' assignOrTick Map.empty enemies

    assignOrTick acc eid enemy
      | enemyHealth enemy <= 0 =
          let current = Map.findWithDefault (enemyHitDuration + enemyDeathDuration) eid deathTimers
              nextT = max 0 (current - dt)
          in Map.insert eid nextT acc
      | otherwise = acc

    -- Remove expired and set state based on phase
    (enemiesKept, timersKept) = Map.foldlWithKey' decide (enemies, Map.empty) timersTicked

    decide (eMap, tMap) eid t
      | t <= 0 = (Map.delete eid eMap, tMap)
      | otherwise =
          let phaseEnemy =
                case Map.lookup eid eMap of
                  Nothing -> Nothing
                  Just foe ->
                    let foe' = if t > enemyDeathDuration then foe { enemyState = Hit }
                               else foe { enemyState = Dead }
                    in Just foe'
              eMap' = maybe eMap (\foe' -> Map.insert eid foe' eMap) phaseEnemy
              tMap' = Map.insert eid t tMap
          in (eMap', tMap')

-- Apply AoE damage for players who just started an attack this frame.
applyPlayerAoE :: Float -> Map.Map EntityID Player -> Map.Map EntityID Enemy -> Map.Map EntityID Enemy
applyPlayerAoE dt players enemies = foldl' applyForPlayer enemies (Map.elems players)
  where
    -- Trigger at the first frame of Attacking: before timers are ticked we expect playerAttackTimer ~ mageAttackDuration
    threshold = mageAttackDuration - (dt * 0.25)
    applyForPlayer acc player
      | playerState player == Attacking
      , playerAttackTimer player >= threshold = Map.map (damageEnemy player) acc
      | otherwise = acc

    damageEnemy player enemy
      | enemyHealth enemy <= 0 = enemy
      | dist2 <= mageAttackRadius * mageAttackRadius =
          let newHp = max 0 (enemyHealth enemy - mageAttackDamage)
          in enemy { enemyHealth = newHp, enemyState = if newHp > 0 then Hit else Dead }
      | otherwise = enemy
      where
        Vec px py = playerPos player
        Vec ex ey = enemyPos enemy
        dx = px - ex
        dy = py - ey
        dist2 = dx*dx + dy*dy

defaultPlayer :: EntityID -> Player
defaultPlayer pid = Player
  { playerId = pid
  , playerPos = spawnPos
  , playerState = Idle
  , playerDir = spawnDir
  , playerHealth = 100
  , maxHealth = 100
  , playerMana = 100
  , maxMana = 100
  , playerLives = playerDefaultLives
  , skill1Cooldown = 0
  , ultimateCooldown = 0
  , playerAttackTimer = 0
  , playerMage = if odd pid then TP.Mage1 else TP.Mage2  -- PlayerMage
  }
  where
    (spawnPos, spawnDir) = case pid of
      1 -> (Vec (-352) (-76), DirRight)
      2 -> (Vec 352 (-76), DirLeft)
      _ -> (Vec 0 (-76), DirDown)

ensurePlayerExists :: EntityID -> Map.Map EntityID Player -> Map.Map EntityID Player
ensurePlayerExists pid players = Map.alter ensure pid players
  where
    ensure Nothing  = Just (defaultPlayer pid)
    ensure (Just p) = Just p