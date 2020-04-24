module Main where

import Prelude

import Color (white)
import Color.Scheme.X11 (red, whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (d0)
import Data.Vec (modifyAt, vec2, vec3)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Seija.App (AppReader, startApp, version)
import Seija.Asset (Asset2D, fontPath, loadAssetSync, spriteSheetPath, texturePath)
import Seija.Component as C
import Seija.Element (image, spriteB, text)
import Seija.FRP (Behavior, Event, EventType(..), attachFoldBehavior, fetchEvent, foldBehavior, mergeEvent, newBehavior, tagMapBehavior)
import Seija.Foreign (Entity, _windowBgColor, _windowHeight, _windowWidth)
import Seija.Math.Vector (Vector2f)
import Seija.Simple2D (newEventRoot)
import Seija.UI.Buildin.Controls (button)

iRES_PATH :: String
iRES_PATH = "./res/"


main :: Effect Unit
main = do
  error version
  let s2dcfg = default #     (_windowWidth   .~ 1024)
                         >>> (_windowHeight  .~ 768)
                         >>> (_windowBgColor .~ (Just whitesmoke))
  startApp s2dcfg appMain (Just iRES_PATH)
  log "main end"

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



