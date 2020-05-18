module SnakeGame where

import Prelude

import Color (black, rgba', white)
import Data.Array (fromFoldable, (..))
import Data.Int (toNumber)
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (d0, d1)
import Data.Vec (modifyAt, vec2, vec3)
import Effect (Effect)
import Effect.Class.Console (errorShow)
import Effect.Ref as R
import Partial.Unsafe (unsafePartial)
import Seija.App (class IGame, class MonadApp, GameM)
import Seija.Asset (loadAsset)
import Seija.Asset.LoaderInfo (spriteSheetLoaderInfo)
import Seija.Asset.Texture (Filter(..), SamplerDesc(..), TextureConfig(..), WrapMode(..))
import Seija.Asset.Types (SpriteSheet)
import Seija.Component as C
import Seija.Element (listElement, spriteB, sprite_, switchElement, text)
import Seija.FRP (Behavior, Event, UpdateType(..), effectBehavior, fetchGlobalKeyEvent, fetchTimeEvent, foldBehavior, holdBehavior, tagBehavior)
import Seija.FRP as FRP
import Seija.Foreign (Entity, unsafeShow)
import Seija.Math.Vector (Vector3f)
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
  let sheetConfig = TextureConfig { generate_mips:false,  premultiply_alpha:false, sampler_info: SamplerDesc {filter:Nearest,wrap_mode:Clamp } }
  snakeSheet <- loadAsset (spriteSheetLoaderInfo "snake.json" (Just sheetConfig))
  (rootEvent::FRP.Event GameEvent) <- FRP.newEvent
  (dGameData::FRP.Dynamic GameData) <- FRP.foldDynamic newGameData rootEvent handleEvent
  
  (bElement::FRP.Behavior (GameRun Entity)) <- FRP.foldBehavior (mainMenu rootEvent) 
                                                                (FRP.updated dGameData) 
                                                                (\a ea -> gameSateToElement ea.state rootEvent snakeSheet)
  switchElement root bElement
  pure unit

gameSateToElement::GameState -> FRP.Event GameEvent -> SpriteSheet -> GameRun Entity
gameSateToElement MainMenu rootev _       = mainMenu rootev
gameSateToElement Gameing  rootev sheet   = gameScene sheet
gameSateToElement EndMenu  rootev sheet   = gameScene sheet
  

handleEvent::GameData -> GameEvent -> GameData
handleEvent d StartGame = d { state = Gameing }


mainMenu::forall m. MonadApp m => MonadSkin m => FRP.Event GameEvent -> m Entity
mainMenu rootEv = do
  skin <- unsafeAskUISkin
  elBg <- sprite_ skin.defaultSheet "entry" [C.rSize $ vec2 1024.0 768.0,C.imageSlice0Type] Nothing
  elTitle <- text skin.defaultFont [C.tText "Snake Game",C.cColor white,C.rSize $ vec2 400.0 40.0,C.tFontSize 40,C.tPos $ vec3 0.0 180.0 0.0] (Just elBg)
  (eClick /\ elBtn) <- button "Enter" [C.tFontSize 30,C.rSizeVec2 240.0 80.0,C.tPosVec3 0.0 (-200.0) 0.0] (Just elBg)
  FRP.setNextEvent (eClick $> StartGame) rootEv
  pure elBg



initWall::forall m. MonadApp m => MonadSkin m => Entity -> SpriteSheet ->  m Unit
initWall parent sheet = do
  let leftX = -1024.0 * 0.5
  let topY = 768.0 * 0.5
  let leftTop = vec3 (-1024.0 * 0.5) (768.0 * 0.5) 0.0
  let leftBottom = vec3 (-1024.0 * 0.5) ((-768.0) * 0.5) 0.0
  let rightTop = vec3 256.0 (768.0 * 0.5) 0.0
  let rightBottom = vec3 256.0 ((-768.0) * 0.5) 0.0
  let topWallPosArr = map (\n -> modifyAt d0 (add ((toNumber n) * 32.0)) (modifyAt d0 (\x -> x + 32.0) leftTop)) (0..22)
  _ <- for topWallPosArr (\pos -> sprite_ sheet "block-2" [C.rAnchorVec2 0.0 1.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos pos] (Just parent))
  let leftWallPosArr = map (\n -> modifyAt d1 (add ((toNumber n) * (-32.0))) (modifyAt d1 (\y -> y - 32.0) leftTop)) (0..21)
  _ <- for leftWallPosArr (\pos -> sprite_ sheet "block-3" [C.rAnchorVec2 0.0 1.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos pos] (Just parent))
  let bottomWallPosArr = map (\n -> modifyAt d0 (add ((toNumber n) * 32.0)) (modifyAt d0 (\x -> x + 32.0) leftBottom)) (0..22)
  _ <- for bottomWallPosArr (\pos -> sprite_ sheet "block-5" [C.rAnchorVec2 0.0 0.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos pos] (Just parent))

  let rightWallPosArr = map (\n -> modifyAt d1 (add ((toNumber n) * (-32.0))) (modifyAt d1 (add (-64.0)) rightTop)) (0..21)
  _ <- for rightWallPosArr (\pos -> sprite_ sheet "block-4" [C.rAnchorVec2 0.0 0.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos pos] (Just parent))

  --skin <- unsafeAskUISkin
  --let innerGround = concat $ map (\x -> map (\y -> let xx = toNumber x
  --                                                     yy = toNumber y 
  --                                                 in vec3 (leftX + 32.0 + (xx * 32.0)) (topY -32.0 - (yy * 32.0) ) 0.0)  (0..21)) (0..22)
  --_ <- for innerGround (\pos -> text skin.defaultFont [C.tFontSize 10,C.tText (show $ map (\num -> ceil $ num / 32.0 ) $ toArray pos),C.cColor white,C.rSizeVec2 100.0 100.0,C.tPos pos] (Just parent))
  
  --el1 <- sprite_ sheet "s-0" [C.tScaleVec3 4.0 4.0 1.0] (Just parent)
  --el2 <- sprite_ sheet "s-4" [C.tScaleVec3 4.0 4.0 1.0,C.tPosVec3 0.0 (-32.0) 0.0] (Just parent)
  --el4 <- sprite_ sheet "s-4" [C.tScaleVec3 4.0 4.0 1.0,C.tPosVec3 0.0 (-48.0) 0.0] (Just parent)
  
  _ <- sprite_ sheet "block-0" [C.rAnchorVec2 0.0 1.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos leftTop] (Just parent)
  _ <- sprite_ sheet "block-0" [C.rAnchorVec2 0.0 0.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos leftBottom] (Just parent)
  _ <- sprite_ sheet "block-0" [C.rAnchorVec2 0.0 1.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos rightTop] (Just parent)
  _ <- sprite_ sheet "block-0" [C.rAnchorVec2 0.0 0.0,C.tScaleVec3 4.0 4.0 1.0,C.tPos rightBottom] (Just parent)
  pure unit

initScoreBoard::forall m. MonadApp m => MonadSkin m => Entity ->  m Unit
initScoreBoard parent = do
  skin <- unsafeAskUISkin
  _ <- text skin.defaultFont [C.rSizeVec2 100.0 30.0,C.tFontSize 28,C.tText "积分:",C.tPosVec3 340.0 200.0 0.0,C.cColor white] (Just parent)
  _ <- text skin.defaultFont [C.rSizeVec2 100.0 30.0,C.tFontSize 28,C.tText "速度:",C.tPosVec3 340.0 120.0 0.0,C.cColor white] (Just parent)
  pure unit

gameScene:: SpriteSheet ->  GameRun Entity
gameScene sheet = do
  skin <- unsafeAskUISkin
  elBg <- sprite_ sheet "white" [C.rSizeVec2 1024.0 768.0,C.tPosVec3 0.0 0.0 100.0,C.cColor black] Nothing
  initWall elBg sheet
  initScoreBoard elBg
  evKey <- fetchGlobalKeyEvent elBg 
  evTimer <- fetchTimeEvent elBg (Time 0.5)
  bDir <- foldBehavior MoveLeft evKey updateDir
  eTickDir <- tagBehavior bDir evTimer
  bSnake <- foldBehavior (SnakeList $ L.fromFoldable [{key:1,x:11,y:11},{key:2,x:12,y:11},{key:3,x:13,y:11}]) eTickDir updateList
  (eSnake::Event SnakeList) <- tagBehavior bSnake evTimer
  (bNode::Behavior (Array SnakeNode)) <- holdBehavior [] (snakeListToArray <$> eSnake)
  listElement elBg bNode (createSnakeNode sheet elBg eSnake eTickDir)
  pure elBg

createSnakeNode::SpriteSheet -> Entity -> Event SnakeList -> Event MoveDir  ->  SnakeNode -> GameRun Entity
createSnakeNode sheet p eSnake eDir {key,x,y}  = do
  bPos <- foldBehavior (gridToPos x y) eSnake (\a (SnakeList lst) ->
                                                    let node = unsafePartial $ fromJust $ L.find (\n -> n.key == key) lst
                                                    in 
                                                      gridToPos node.x node.y
                                                    
                                                )
  bSpriteName <- foldBehavior (if key == 1 then "s-1" else "s-4") eDir (\a ea -> 
                                        if key == 1 
                                         then dirToPic ea
                                         else "s-4"
                                     )
  spriteB sheet bSpriteName [C.tScaleVec3 4.0 4.0 1.0,C.tPosB bPos,C.rAnchorVec2 0.0 1.0] (Just p)

dirToPic::MoveDir -> String
dirToPic MoveLeft = "s-1"
dirToPic MoveRight = "s-3"
dirToPic MoveUp = "s-0"
dirToPic MoveDown = "s-2"

gridToPos::Int -> Int -> Vector3f
gridToPos x y = vec3 (xx * 32.0 + leftX) (topY - yy * 32.0) (-0.1)
  where
    leftX = -512.0 + 32.0
    topY  = 384.0 - 32.0
    xx    = toNumber x
    yy    = toNumber y

snakeListToArray::SnakeList -> Array SnakeNode
snakeListToArray (SnakeList lst) = fromFoldable lst

updateDir::MoveDir -> Tuple Int Boolean -> MoveDir
updateDir dir (10 /\ true) = if isRight dir then MoveRight else MoveLeft
updateDir dir (13 /\ true) = if isLeft dir then MoveLeft else MoveRight
updateDir dir (32 /\ true) = if isDown dir then MoveDown else MoveUp
updateDir dir (28 /\ true) = if isUp dir then MoveUp else MoveDown 
updateDir dir _  = dir

updateList::SnakeList -> MoveDir -> SnakeList
updateList (SnakeList lst@(x:xs)) dir = SnakeList $ (updateFirst x dir):(unsafePartial $ map updateKey $ fromJust  (L.init lst) )
updateList lst dir = lst

updateKey::SnakeNode -> SnakeNode
updateKey n@{key} = n {key = key + 1}

updateFirst :: SnakeNode-> MoveDir -> SnakeNode
updateFirst {key,x,y}  MoveLeft = {key:key, x: x - 1, y:y } 
updateFirst {key,x,y}  MoveRight = {key:key,x:x + 1, y:y } 
updateFirst {key,x,y}  MoveUp = {key:key,x:x, y:y - 1 } 
updateFirst {key,x,y}  MoveDown = {key:key,x:x, y:y + 1 } 


data MoveDir = MoveLeft | MoveRight | MoveUp | MoveDown
isLeft::MoveDir -> Boolean
isLeft MoveLeft = true
isLeft _ = false

isRight::MoveDir -> Boolean
isRight MoveRight = true
isRight _ = false

isUp::MoveDir -> Boolean
isUp MoveUp = true
isUp _ = false

isDown::MoveDir -> Boolean
isDown MoveDown = true
isDown _ = false

instance showMoveDir :: Show MoveDir where
  show MoveLeft = "MoveLeft"
  show MoveRight = "MoveRight"
  show MoveUp = "MoveUp"
  show MoveDown = "MoveDown"

type SnakeNode =  {
  key::Int,
  x::Int,
  y::Int
}

newtype SnakeList = SnakeList (L.List SnakeNode)


instance showSnakeList :: Show SnakeList where
  show (SnakeList arr) = show arr