module Main where

import Color.Scheme.X11 (whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, logShow)
import Effect.Console (log)
import Prelude (Unit, bind, discard, map, pure, show, unit, (#), ($), (>>>), ($>))
import Seija.App (AppReader, startApp, version)
import Seija.Asset (loadAssetSync, texturePath)
import Seija.Component as C
import Seija.Element (image)
import Seija.FRP (Event, EventType(..), effectEvent, fetchEvent)
import Seija.Foreign (_windowBgColor, _windowHeight, _windowWidth)
import Seija.Math.Vector (zeroVec3)
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
  img <- image asset [C.tPos zeroVec3] (Just root)
  ev <- fetchEvent img Click false
  --bNum <-holdFoldBehavior 0 ev
  let evString = ev $> "evString"
  let evString2 = evString $> "evString2"
  liftEffect $ do
    effectEvent evString2 error
  liftEffect $ log "Exit AppMain"