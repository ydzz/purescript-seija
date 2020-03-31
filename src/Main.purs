module Main where

import Data.Default (default)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Console (log)
import Prelude (Unit, discard, pure, unit, ($))
import Seija.App (version)
import Seija.Foreign (AppConfig(..), newApp, newSimple2d, runApp)

main :: Effect Unit
main = do
  error version
  let s2d = newSimple2d default
  let (AppConfig a)  = default
  let (appConfig::AppConfig) = AppConfig $ a {onStart = Just onStart,onUpdate = Just onUpdate}
  let app = newApp s2d appConfig
  runApp app
  log "main end"

onStart::Effect Unit
onStart = do
  error "OnStart"

onUpdate::Effect Unit
onUpdate = do
  error "onUpdate"
  pure unit