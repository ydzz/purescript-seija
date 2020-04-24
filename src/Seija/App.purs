module Seija.App where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Seija.Foreign (AppConfig(..), Loader, Simple2dConfig, World, appVersion, fetchLoader, loadAssetSync, newApp, newSimple2d, runApp)

version::String
version = appVersion

type AppHandler = {
    world::  World,
    loader:: Loader
}

type AppReader = ReaderT AppHandler Effect



startApp::Simple2dConfig -> AppReader Unit -> Maybe String -> Effect Unit
startApp s2dcfg start mayPath = do
    let appConfig = AppConfig {
                                onStart:  onStartApp start,
                                onUpdate: onUpdate,
                                onQuit:   onQuit,
                                resPath:mayPath
                            }
    let s2d = newSimple2d s2dcfg
    let app = newApp s2d appConfig
    runApp app

onStartApp::AppReader Unit -> World -> Effect Unit
onStartApp r w = do
    loader <- fetchLoader w
    runReaderT r {world : w,loader}

onUpdate::World -> Effect Boolean
onUpdate world = do
    pure true

onQuit::World -> Effect Unit
onQuit world = do
    pure unit

askWorld::AppReader World
askWorld = do
    handle <- ask
    pure handle.world