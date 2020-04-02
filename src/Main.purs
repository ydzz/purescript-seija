module Main where
import Color.Scheme.X11 (whitesmoke)
import Data.Default (default)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, logShow)
import Effect.Console (log)
import Prelude (Unit, bind, discard, (#), ($), (>>>))
import Seija.App (AppReader, startApp, version)
import Seija.Asset (loadAssetSync, texturePath)
import Seija.Foreign (_windowBgColor, _windowHeight, _windowWidth)
import Seija.Simple2D (newEventRoot, newImage)

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
  assetid <- loadAssetSync (texturePath "b.jpg")
  assetid2 <- loadAssetSync (texturePath "a.jpg")
  img <- newImage assetid (Just root)
  --img2 <- newImage assetid2 (Just root) { trans:[], rect: [] ,image: []}
  liftEffect $ do
   logShow assetid
   logShow assetid2
  liftEffect $ log "Exit AppMain"
