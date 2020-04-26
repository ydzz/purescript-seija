module Main where

import Prelude
import Color.Scheme.X11 (whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Vec (vec3)
import Effect (Effect)
import Effect.Class.Console (error, errorShow)
import Effect.Console (log)
import Effect.Ref as R
import Seija.App (class IGame, GameM, startApp, version)
import Seija.Component as C
import Seija.FRP (effectEvent)
import Seija.Foreign (_windowBgColor, _windowHeight, _windowWidth)
import Seija.Simple2D (newEventRoot)
import Seija.UI.Buildin.Controls (class HasUISkin, UISkin, button, checkBox, loadSkin)

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

type GameRun = GameM TestGame Effect

instance uiSkinTestGame :: HasUISkin TestGame where
  askSkinRef (TestGame t) = t.skinRef

main :: Effect Unit
main = do
  error version
  let s2dcfg = default #     (_windowWidth   .~ 1024)
                         >>> (_windowHeight  .~ 768)
                         >>> (_windowBgColor .~ (Just whitesmoke))
  testGame <- defaultTestGame 
  startApp s2dcfg testGame gameMain
  log "main end"



gameMain::GameRun Unit
gameMain = do
  error "Enter GameMain"
  root <- newEventRoot
  loadSkin
  (evCheck /\ el) <- checkBox false [] (Just root)
  effectEvent evCheck errorShow
  _  <- button "加一" [C.tPos $ vec3 (80.0) 0.0 0.0]  (Just root)
  _  <- button "减一" [C.tPos $ vec3 (-80.0) 0.0 0.0] (Just root)
  pure unit

snakeMain::GameRun Unit
snakeMain = do
  
  pure unit
{-
appMain::AppReader Unit
appMain = do
  root <- newEventRoot
  asset <- loadAssetSync (texturePath "b.jpg")
  sheet <- loadAssetSync (spriteSheetPath "material.json")
  font <- loadAssetSync (fontPath "WenQuanYiMicroHei.ttf")
  elBtn <- button sheet font "-" [C.rSize $ vec2 100.0 100.0,C.tPos $ vec3 (-100.0) 0.0 0.0] (Just root)
  elBtn2 <- button sheet font "+" [C.rSize $ vec2 100.0 100.0,C.tPos $ vec3 100.0 0.0 0.0] (Just root)
  pure unit


test0::Asset2D -> Asset2D -> Entity -> AppReader Unit
test0 sheet font root = do
  (eClick /\ spr) <- testSprite sheet root
  bNum::Behavior Int <- liftEffect $ foldBehavior 0 (eClick $> 1) (\a ea -> ea + a)
  bText::Behavior String <- liftEffect $ tagMapBehavior bNum eClick show
  _ <- text font [C.rSize $ vec2 100.0 25.0,C.tTextB bText,C.tColor white] (Just spr)
  liftEffect $ errorShow font

testSprite::Asset2D -> Entity -> AppReader (Tuple (Event Int) Entity)
testSprite asset root = do
  let bSpriteName = newBehavior "button"
  spr <- spriteB asset bSpriteName [C.rSize $ vec2 80.0 30.0,C.imageSlice0Type] (Just root)
  evDown::Event Int <- fetchEvent spr TouchStart false
  evUp::Event Int   <- fetchEvent spr TouchEnd false
  evEnter::Event Int   <- fetchEvent spr MouseEnter false
  evLeave::Event Int   <- fetchEvent spr MouseLeave false
  evClick::Event Int   <- fetchEvent spr Click false
  liftEffect do
    v <- Ref.new $ newBehavior "Fucker"
    mEv <- mergeEvent [evDown $> "button-active",evUp $> "button",evEnter $> "button-hover",evLeave $> "button"]
    attachFoldBehavior mEv bSpriteName (\a ea -> ea)
    pure (evClick /\ spr)

testImage::Asset2D -> Entity -> AppReader Unit
testImage asset root = do
  let (bSize::Behavior Vector2f) = newBehavior $ vec2 100.0 100.0
  img <- image asset [C.rSizeB bSize,C.cColor red] (Just root)
  ev <- fetchEvent img Click false
  liftEffect do
    attachFoldBehavior ev bSize (\val ea -> modifyAt d0 (add 1.0) val)
    --let evString = ev $> "evString"
    --bNum <- foldBehavior "IDLE" evString (\val e -> val <> e)
    --effectBehavior bNum warn
    log "Exit AppMain"
-}