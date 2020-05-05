module SnakeGame where

import Prelude

import Color (black, rgba, white)
import Color.Scheme.X11 (gray, red)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2, vec3)
import Effect (Effect)
import Effect.Class.Console (errorShow)
import Effect.Ref as R
import Seija.App (class IGame, class MonadApp, GameM)
import Seija.Asset (Asset2D(..), loadAssetSync, spriteSheetPath)
import Seija.Component as C
import Seija.Element (emptyElement, sprite_, switchElement, text)
import Seija.FRP as FRP
import Seija.Foreign (Entity)
import Seija.Simple2D (newEventRoot)
import Seija.UI.Buildin.Controls (class HasUISkin, class MonadSkin, UISkin, button, loadSkin, unsafeAskUISkin)

iRES_PATH :: String
iRES_PATH = "./res/"

data TestGame = TestGame {
  skinRef:: R.Ref (Maybe UISkin)
}

defaultTestGame::Effect TestGame
defaultTestGame = do
  ref <- R.new Nothing
  pure $ TestGame {skinRef:ref}

instance igameTestGame :: IGame TestGame where
  resPath _ = (Just iRES_PATH)

instance uiSkinTestGame :: HasUISkin TestGame where
  askSkinRef (TestGame t) = t.skinRef

type GameRun = GameM TestGame Effect

data GameState = MainMenu | Gameing | EndMenu

data GameEvent = StartGame

instance showGameEvent :: Show GameEvent where
  show StartGame = "StartGame"

instance showGameState :: Show GameState where
  show MainMenu = "MainMenu"
  show Gameing  = "Gameing"
  show EndMenu  = "EndMenu"

type GameData = {
    state::GameState
}

newGameData :: GameData
newGameData = { state:MainMenu }

snakeMain::GameRun Unit
snakeMain = do
  loadSkin
  skin <- unsafeAskUISkin
  root <- newEventRoot
  snakeSheet <- loadAssetSync (spriteSheetPath "snake.json")
  errorShow snakeSheet
  (rootEvent::FRP.Event GameEvent) <- FRP.newEvent
  (dGameData::FRP.Dynamic GameData) <- FRP.foldDynamic newGameData rootEvent handleEvent
  evBoxStart <- FRP.newEventBox
  (bElement::FRP.Behavior (GameRun Entity)) <- FRP.foldBehavior (mainMenu evBoxStart) 
                                                                (FRP.updated dGameData) 
                                                                (\a ea -> gameSateToElement ea.state evBoxStart snakeSheet)
  switchElement root bElement
  evClickStart <- FRP.unsafeUnEventBox evBoxStart
  FRP.setNextEvent (evClickStart $> StartGame) rootEvent

  pure unit

gameSateToElement::GameState -> FRP.EventBox Entity -> Asset2D -> GameRun Entity
gameSateToElement MainMenu evBox _       = mainMenu evBox
gameSateToElement Gameing  evBox sheet   = gameScene sheet
gameSateToElement EndMenu  evBox sheet   = gameScene sheet
  

handleEvent::GameData -> GameEvent -> GameData
handleEvent d StartGame = d { state = Gameing }


mainMenu::forall m. MonadApp m => MonadSkin m => FRP.EventBox Entity -> m Entity
mainMenu evBox = do
  skin <- unsafeAskUISkin
  elBg <- sprite_ skin.defaultSheet "entry" [C.rSize $ vec2 1024.0 768.0,C.imageSlice0Type] Nothing
  _ <- text skin.defaultFont [C.tText "贪     食     蛇",C.cColor white,C.rSize $ vec2 400.0 40.0,C.tFontSize 40,C.tPos $ vec3 0.0 180.0 0.0] (Just elBg)
  (eClick /\ elBtn) <- button "开始游戏" [C.tFontSize 30,C.rSizeVec2 240.0 80.0,C.tPosVec3 0.0 (-200.0) 0.0] (Just elBg)
  FRP.putEventBox eClick evBox
  pure elBg


gameScene::forall m. MonadApp m => MonadSkin m =>Asset2D ->  m Entity
gameScene sheet = do
  skin <- unsafeAskUISkin
  elBg <- sprite_ sheet "white" [C.rSizeVec2 1024.0 768.0,C.cColor black] Nothing
  el0 <- sprite_ sheet "a-0" [C.tScale $ vec3 4.0 4.0 1.0] (Just elBg)
  el1 <- sprite_ sheet "a-1" [C.tScale $ vec3 4.0 4.0 1.0,C.tPosVec3 32.0 0.0 0.0] (Just elBg)
  pure elBg