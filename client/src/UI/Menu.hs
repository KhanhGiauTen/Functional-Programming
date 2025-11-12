module UI.Menu
  ( MenuState(..)
  , LoginState(..)
  , MainMenuState(..)
  , CharacterSelectState(..)
  , LobbyState(..)
  , LobbyPlayer(..)
  , ActiveField(..)
  , MageOption(..)
  , LoginHover(..)
  , MainHover(..)
  , CharHover(..)
  , LobbyHover(..)
  , MenuEvent(..)
  , MenuInput(..)
  , initialMenuState
  , initialMainMenu
  , initialCharacterSelect
  , initialLobbyState
  , updateMenuState
  , handleMenuInput
  , setLoginStatus
  , setMainMessage
  , setCharacterStatus
  , setLobbyStatus
  , loginPanelRect
  , loginUsernameRect
  , loginPasswordRect
  , loginLoginButtonRect
  , loginRegisterButtonRect
  , mainPanelRect
  , mainPlayRect
  , mainSelectRect
  , mainLogoutRect
  , characterPanelRect
  , characterConfirmRect
  , characterBackRect
  , lobbyPanelRect
  , lobbyStartRect
  , lobbyBackRect
  , lobbyReadyRect
  , mageCardRect
  , mageSlots
  , Rect(..)
  , pointInsideRect
  , mageLabel
  ) where

import Data.Char (isPrint)
import Data.List (foldl')

-- | Menu state machine ======================================================

data MenuState
  = MenuLogin LoginState
  | MenuMain MainMenuState
  | MenuCharacter CharacterSelectState
  | MenuLobby LobbyState
  deriving (Eq, Show)

data ActiveField
  = FieldUsername
  | FieldPassword
  deriving (Eq, Show)

data LoginState = LoginState
  { loginUsername    :: String
  , loginPassword    :: String
  , loginStatus      :: Maybe String
  , loginActiveField :: ActiveField
  , loginCaretPhase  :: Float
  , loginHover       :: Maybe LoginHover
  } deriving (Eq, Show)

data LoginHover
  = HoverUsername
  | HoverPassword
  | HoverLoginButton
  | HoverRegisterButton
  deriving (Eq, Show)

data MainMenuState = MainMenuState
  { mainUsername :: String
  , mainMessage  :: Maybe String
  , mainHover    :: Maybe MainHover
  } deriving (Eq, Show)

data MainHover
  = HoverPlay
  | HoverCharacters
  | HoverLogout
  deriving (Eq, Show)

data CharacterSelectState = CharacterSelectState
  { csOptions    :: [MageOption]
  , csSelected   :: Maybe MageOption
  , csStatus     :: Maybe String
  , csHover      :: Maybe CharHover
  , csBlinkPhase :: Float
  } deriving (Eq, Show)

data CharHover
  = HoverMage MageOption
  | HoverConfirm
  | HoverBack
  deriving (Eq, Show)

data LobbyPlayer = LobbyPlayer
  { lpName  :: String
  , lpMage  :: Maybe MageOption -- local lobby placeholder; will be replaced by playerMage from GameState
  , lpReady :: Bool
  } deriving (Eq, Show)

data LobbyState = LobbyState
  { lobbyRoomId :: String
  , lobbyPlayers :: [LobbyPlayer]
  , lobbyMyIndex :: Int
  , lobbyHover :: Maybe LobbyHover
  , lobbyStatus :: Maybe String
  } deriving (Eq, Show)

data LobbyHover
  = HoverStart
  | HoverBackLobby
  | HoverReady
  deriving (Eq, Show)

data MageOption
  = Mage1
  | Mage2
  deriving (Eq, Show, Enum, Bounded)

data MenuEvent
  = MenuNoEvent
  | MenuAttemptLogin String String
  | MenuAttemptRegister String String
  | MenuPlayQuick
  | MenuOpenCharacters
  | MenuLogout
  | MenuSelectMage MageOption
  | MenuConfirmMage MageOption
  | MenuBackToMain
  | MenuStartGame
  | MenuToggleReady
  deriving (Eq, Show)

data MenuInput
  = InputMouseMove Float Float
  | InputMouseClick Float Float
  | InputText String
  | InputBackspace
  | InputTab
  | InputEnter
  | InputEscape
  deriving (Eq, Show)

-- | Geometry (center-based coordinates, width/height) =======================

data Rect = Rect
  { rectCenter :: (Float, Float)
  , rectSize   :: (Float, Float)
  } deriving (Eq, Show)

loginPanelRect :: Rect
loginPanelRect = Rect (0, 30) (620, 620)

loginUsernameRect, loginPasswordRect, loginLoginButtonRect, loginRegisterButtonRect :: Rect
loginUsernameRect = Rect (0, 50) (320, 40)
loginPasswordRect = Rect (0, -40) (320, 40)
loginLoginButtonRect = Rect (-150, -190) (220, 64)
loginRegisterButtonRect = Rect (150, -190) (220, 64)

mainPanelRect :: Rect
mainPanelRect = Rect (0, 20) (460, 440)

mainPlayRect, mainSelectRect, mainLogoutRect :: Rect
mainPlayRect = Rect (0, 80) (260, 70)
mainSelectRect = Rect (0, 0) (260, 70)
mainLogoutRect = Rect (0, -80) (260, 70)

characterPanelRect :: Rect
characterPanelRect = Rect (0, 30) (640, 480)

characterConfirmRect, characterBackRect :: Rect
characterConfirmRect = Rect (0, -200) (220, 60)
characterBackRect = Rect (0, -260) (220, 56)

lobbyPanelRect :: Rect
lobbyPanelRect = Rect (0, 30) (640, 480)

lobbyStartRect, lobbyBackRect, lobbyReadyRect :: Rect
lobbyStartRect = Rect (0, -200) (220, 60)
lobbyBackRect = Rect (0, -260) (220, 56)
lobbyReadyRect = Rect (0, -140) (220, 60)

mageCardRect :: Float -> Rect
mageCardRect centerX = Rect (centerX, 60) (200, 230)

mageSlots :: Int -> [Float]
mageSlots n =
  let count = max 1 n
      spacing = 220
      total = fromIntegral (count - 1) * spacing
      start = -total / 2
  in [ start + fromIntegral idx * spacing | idx <- [0 .. count - 1] ]

pointInsideRect :: (Float, Float) -> Rect -> Bool
pointInsideRect (px, py) (Rect (cx, cy) (w, h)) =
  abs (px - cx) <= w / 2 && abs (py - cy) <= h / 2

-- | Constructors ============================================================

initialMenuState :: MenuState
initialMenuState = MenuLogin defaultLoginState

initialMainMenu :: String -> MenuState
initialMainMenu name = MenuMain MainMenuState
  { mainUsername = name
  , mainMessage = Nothing
  , mainHover = Nothing
  }

initialCharacterSelect :: [MageOption] -> MenuState
initialCharacterSelect options = MenuCharacter CharacterSelectState
  { csOptions = if null options then [Mage1, Mage2] else take 2 options
  , csSelected = Nothing
  , csStatus = Nothing
  , csHover = Nothing
  , csBlinkPhase = 0
  }

initialLobbyState :: String -> MageOption -> MenuState
initialLobbyState username mage = MenuLobby LobbyState
  { lobbyRoomId = "ROOM-001"
  , lobbyPlayers =
      [ LobbyPlayer username (Just mage) True
      , LobbyPlayer "Guest" (Just nextMage) True
      ]
  , lobbyMyIndex = 0
  , lobbyHover = Nothing
  , lobbyStatus = Nothing
  }
  where
    nextMage = case mage of
      Mage1 -> Mage2
      Mage2 -> Mage1

-- | External helpers to set messages =======================================

setLoginStatus :: Maybe String -> MenuState -> MenuState
setLoginStatus msg (MenuLogin st) = MenuLogin st { loginStatus = msg }
setLoginStatus _ state = state

setMainMessage :: Maybe String -> MenuState -> MenuState
setMainMessage msg (MenuMain st) = MenuMain st { mainMessage = msg }
setMainMessage _ state = state

setCharacterStatus :: Maybe String -> MenuState -> MenuState
setCharacterStatus msg (MenuCharacter st) = MenuCharacter st { csStatus = msg }
setCharacterStatus _ state = state

setLobbyStatus :: Maybe String -> MenuState -> MenuState
setLobbyStatus msg (MenuLobby st) = MenuLobby st { lobbyStatus = msg }
setLobbyStatus _ state = state

-- | State updates ===========================================================

updateMenuState :: Float -> MenuState -> MenuState
updateMenuState dt state =
  case state of
    MenuLogin st -> MenuLogin st { loginCaretPhase = wrap (loginCaretPhase st + dt) }
    MenuCharacter st -> MenuCharacter st { csBlinkPhase = wrap (csBlinkPhase st + dt) }
    _ -> state
  where
    wrap x
      | x >= 1 = x - 1
      | otherwise = x

-- | Input handling ==========================================================

handleMenuInput :: MenuInput -> MenuState -> (MenuState, MenuEvent)
handleMenuInput input state =
  case state of
    MenuLogin st -> first MenuLogin (handleLogin input st)
    MenuMain st -> first MenuMain (handleMain input st)
    MenuCharacter st -> first MenuCharacter (handleCharacter input st)
    MenuLobby st -> first MenuLobby (handleLobby input st)
  where
    first ctor (st', evt) = (ctor st', evt)

handleLogin :: MenuInput -> LoginState -> (LoginState, MenuEvent)
handleLogin input st =
  case input of
    InputMouseMove x y -> (st { loginHover = identifyLoginHover (x, y) }, MenuNoEvent)
    InputMouseClick x y ->
      case identifyLoginHover (x, y) of
        Just HoverUsername -> (st { loginActiveField = FieldUsername }, MenuNoEvent)
        Just HoverPassword -> (st { loginActiveField = FieldPassword }, MenuNoEvent)
        Just HoverLoginButton -> (st, MenuAttemptLogin (loginUsername st) (loginPassword st))
        Just HoverRegisterButton -> (st, MenuAttemptRegister (loginUsername st) (loginPassword st))
        Nothing -> (st, MenuNoEvent)
    InputText txt ->
      let trimmed = filter isEditable txt
          st' = foldl' (\acc c -> modifyActiveField (appendChar c) acc) st trimmed
      in (st', MenuNoEvent)
    InputBackspace -> (modifyActiveField removeLast st, MenuNoEvent)
    InputTab -> (st { loginActiveField = toggleField (loginActiveField st) }, MenuNoEvent)
    InputEnter -> (st, MenuAttemptLogin (loginUsername st) (loginPassword st))
    InputEscape -> (st, MenuNoEvent)
  where
    isEditable c = isPrint c && c /= '\r' && c /= '\n'

handleMain :: MenuInput -> MainMenuState -> (MainMenuState, MenuEvent)
handleMain input st =
  case input of
    InputMouseMove x y -> (st { mainHover = identifyMainHover (x, y) }, MenuNoEvent)
    InputMouseClick x y ->
      case identifyMainHover (x, y) of
        Just HoverPlay -> (st, MenuPlayQuick)
        Just HoverCharacters -> (st, MenuOpenCharacters)
        Just HoverLogout -> (st, MenuLogout)
        Nothing -> (st, MenuNoEvent)
    InputEscape -> (st, MenuLogout)
    _ -> (st, MenuNoEvent)

handleCharacter :: MenuInput -> CharacterSelectState -> (CharacterSelectState, MenuEvent)
handleCharacter input st =
  case input of
    InputMouseMove x y -> (st { csHover = identifyCharacterHover st (x, y) }, MenuNoEvent)
    InputMouseClick x y ->
      case identifyCharacterHover st (x, y) of
        Just (HoverMage mage) -> (st { csSelected = Just mage, csStatus = Nothing }, MenuSelectMage mage)
        Just HoverConfirm ->
          case csSelected st of
            Just mage -> (st, MenuConfirmMage mage)
            Nothing -> (st { csStatus = Just "Please choose a mage." }, MenuNoEvent)
        Just HoverBack -> (st, MenuBackToMain)
        Nothing -> (st, MenuNoEvent)
    InputEscape -> (st, MenuBackToMain)
    _ -> (st, MenuNoEvent)

handleLobby :: MenuInput -> LobbyState -> (LobbyState, MenuEvent)
handleLobby input st =
  case input of
    InputMouseMove x y -> (st { lobbyHover = identifyLobbyHover (x, y) }, MenuNoEvent)
    InputMouseClick x y ->
      case identifyLobbyHover (x, y) of
        Just HoverStart -> (st, MenuStartGame)
        Just HoverBackLobby -> (st, MenuBackToMain)
        Just HoverReady -> (toggleReady st, MenuToggleReady)
        Nothing -> (st, MenuNoEvent)
    InputEscape -> (st, MenuBackToMain)
    _ -> (st, MenuNoEvent)
  where
    toggleReady lobby =
      let idx = lobbyMyIndex lobby
          flipReady i player
            | i == idx = player { lpReady = not (lpReady player) }
            | otherwise = player
          newPlayers = zipWith flipReady [0..] (lobbyPlayers lobby)
      in lobby { lobbyPlayers = newPlayers }

-- | Hover detection =========================================================

identifyLoginHover :: (Float, Float) -> Maybe LoginHover
identifyLoginHover pos
  | pointInsideRect pos loginUsernameRect = Just HoverUsername
  | pointInsideRect pos loginPasswordRect = Just HoverPassword
  | pointInsideRect pos loginLoginButtonRect = Just HoverLoginButton
  | pointInsideRect pos loginRegisterButtonRect = Just HoverRegisterButton
  | otherwise = Nothing

identifyMainHover :: (Float, Float) -> Maybe MainHover
identifyMainHover pos
  | pointInsideRect pos mainPlayRect = Just HoverPlay
  | pointInsideRect pos mainSelectRect = Just HoverCharacters
  | pointInsideRect pos mainLogoutRect = Just HoverLogout
  | otherwise = Nothing

identifyCharacterHover :: CharacterSelectState -> (Float, Float) -> Maybe CharHover
identifyCharacterHover st pos
  | pointInsideRect pos characterConfirmRect = Just HoverConfirm
  | pointInsideRect pos posBack = Just HoverBack
  | otherwise = findSlot (csOptions st) (mageSlots (length (csOptions st)))
  where
    posBack = characterBackRect
    findSlot [] [] = Nothing
    findSlot (m:ms) (x:xs)
      | pointInsideRect pos (mageCardRect x) = Just (HoverMage m)
      | otherwise = findSlot ms xs
    findSlot _ _ = Nothing

identifyLobbyHover :: (Float, Float) -> Maybe LobbyHover
identifyLobbyHover pos
  | pointInsideRect pos lobbyStartRect = Just HoverStart
  | pointInsideRect pos lobbyBackRect = Just HoverBackLobby
  | pointInsideRect pos lobbyReadyRect = Just HoverReady
  | otherwise = Nothing

-- | Internal helpers ========================================================

defaultLoginState :: LoginState
defaultLoginState = LoginState
  { loginUsername = ""
  , loginPassword = ""
  , loginStatus = Nothing
  , loginActiveField = FieldUsername
  , loginCaretPhase = 0
  , loginHover = Nothing
  }

modifyActiveField :: (String -> String) -> LoginState -> LoginState
modifyActiveField f st =
  case loginActiveField st of
    FieldUsername -> st { loginUsername = clamp (f (loginUsername st)) }
    FieldPassword -> st { loginPassword = clamp (f (loginPassword st)) }
  where
    clamp txt
      | length txt > maxLen = take maxLen txt
      | otherwise = txt
    maxLen = 18

appendChar :: Char -> String -> String
appendChar c txt = txt ++ [c]

removeLast :: String -> String
removeLast [] = []
removeLast xs = init xs

toggleField :: ActiveField -> ActiveField
toggleField FieldUsername = FieldPassword
toggleField FieldPassword = FieldUsername

-- | Mage descriptions used by the SDL renderer ==============================
mageLabel :: MageOption -> String
mageLabel Mage1 = "Mage 1"
mageLabel Mage2 = "Mage 2"

