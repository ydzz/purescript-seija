module Main where

import Prelude

import Color.Scheme.X11 (red, whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0)
import Data.Vec (vec2, modifyAt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Console (log)
import Seija.App (AppReader, startApp, version)
import Seija.Asset (loadAssetSync, texturePath)
import Seija.Component as C
import Seija.Element (image)
import Seija.FRP (Behavior, EventType(..), attachFoldBehavior, fetchEvent, newBehavior)
import Seija.Foreign (_windowBgColor, _windowHeight, _windowWidth)
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
  assetid2 <- loadAssetSync (texturePath "a.jpg")
  let (bSize::Behavior Vector2f) = newBehavior $ vec2 100.0 100.0
  img <- image assetid2 [C.rSizeB bSize,C.iColor red] (Just root)
  ev <- fetchEvent img Click false
  liftEffect do
    attachFoldBehavior ev bSize (\val ea -> modifyAt d0 (add 1.0) val)
    --let evString = ev $> "evString"
    --bNum <- foldBehavior "IDLE" evString (\val e -> val <> e)
    --effectBehavior bNum warn
    log "Exit AppMain"
