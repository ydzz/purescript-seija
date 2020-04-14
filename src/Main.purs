module Main where

import Prelude

import Color (white)
import Color.Scheme.X11 (red, whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0)
import Data.Vec (modifyAt, vec2, vec3)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Console (errorShow, log)
import Seija.App (AppReader, startApp, version)
import Seija.Asset (Asset2D, fontPath, loadAssetSync, spriteSheetPath, texturePath)
import Seija.Component as C
import Seija.Element (image, sprite, text)
import Seija.FRP (Behavior, EventType(..), attachFoldBehavior, fetchEvent, newBehavior)
import Seija.Foreign (Entity, _windowBgColor, _windowHeight, _windowWidth)
import Seija.Math.Vector (Vector2f)
import Seija.Simple2D (newEventRoot)

iRES_PATH :: String
iRES_PATH = "/home/yangdao/Project/Rust/seija/examples/first/res/"

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
  liftEffect $ log "Enter AppMain"
  root <- newEventRoot
  asset <- loadAssetSync (texturePath "b.jpg")
  sheet <- loadAssetSync (spriteSheetPath "111/material.json")
  font <- loadAssetSync (fontPath "WenQuanYiMicroHei.ttf")
  spr <- testSprite sheet root
  _ <- text font [C.tPos $ vec3 (-13.0) (-8.0) 0.0,C.rSize $ vec2 100.0 25.0,C.tText "确定",C.tColor white] (Just spr)
  liftEffect $ do
    errorShow sheet
  pure unit


testImage::Asset2D -> Entity -> AppReader Unit
testImage asset root = do
  let (bSize::Behavior Vector2f) = newBehavior $ vec2 100.0 100.0
  img <- image asset [C.rSizeB bSize,C.iColor red] (Just root)
  ev <- fetchEvent img Click false
  liftEffect do
    attachFoldBehavior ev bSize (\val ea -> modifyAt d0 (add 1.0) val)
    --let evString = ev $> "evString"
    --bNum <- foldBehavior "IDLE" evString (\val e -> val <> e)
    --effectBehavior bNum warn
    log "Exit AppMain"


testSprite::Asset2D -> Entity -> AppReader Entity
testSprite asset root = do
  spr <- sprite asset "button-active" [C.imageSlice0Type,C.rSize $ vec2 80.0 30.0] (Just root)
  pure spr
