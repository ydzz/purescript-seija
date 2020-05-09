module Seija.App where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadTrans, ReaderT(..), ask, lift, runReaderT)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Seija.Foreign (AppConfig(..), Simple2dConfig, World, appVersion, newApp, newSimple2d, runApp)

version::String
version = appVersion

type AppHandler = {
    world::  World
}

newtype GameEnv a = GameEnv {
    appHandler::AppHandler,
    env::a
}
derive instance functorGameEnv ::Functor GameEnv
instance applyGameEnv:: Apply GameEnv where
    apply (GameEnv f) (GameEnv fa)  = GameEnv {
        appHandler:fa.appHandler,
        env:f.env  fa.env
    }

newtype GameM r m a = GameM (ReaderT (GameEnv r) m a)

derive instance functorGameM ::(Monad m) => Functor (GameM r m)
instance applyGameM::(Monad m) => Apply (GameM r m) where
    apply (GameM ma) (GameM mb) = GameM (ma <*> mb)

instance applicativeGameM :: Monad m => Applicative (GameM r m) where
    pure = GameM <<< ReaderT <<< const <<< pure

instance bindGameM :: Monad m => Bind (GameM r m) where
  bind (GameM ma) f = GameM $ ma >>= (\a -> let (GameM mb) = f a in mb)

instance monadGameM :: Monad m => Monad (GameM r m)

instance monadTransGameM :: MonadTrans (GameM r) where
  lift = GameM <<< ReaderT <<< const

instance monadEffectGameM :: (MonadEffect m) => MonadEffect (GameM r m) where
  liftEffect = lift <<< liftEffect

instance monadAskGameM :: Monad m => MonadAsk (GameEnv r) (GameM r m) where
  ask = GameM $ ReaderT $ pure
    
instance monadAppHandler::(Monad m) => MonadAppHandler  (GameM r m) where
    askAppHandler = do
       (GameEnv env) <- ask
       pure env.appHandler

class IGame a where
    resPath::a -> Maybe String

class (Monad m) <= MonadAppHandler m where
    askAppHandler::m AppHandler

class (MonadEffect m,MonadAppHandler m) <= MonadApp m
instance monadAppGameM ::(MonadEffect m) => MonadApp (GameM r m)

askEnv::forall r m.(Monad m) => GameM r m r
askEnv = do
   (GameEnv env) <- ask
   pure env.env

startApp::forall a.(IGame a) => Simple2dConfig -> a -> GameM a Effect Unit -> Effect Unit
startApp s2dCfg game main = do
    let appConfig = AppConfig {
                                onStart:  onStartApp main game,
                                onUpdate: onUpdate,
                                onQuit:   onQuit,
                                resPath:  (resPath game)
                              }
    let s2d = newSimple2d s2dCfg
    let app = newApp s2d appConfig
    runApp app

onStartApp::forall  r. GameM r Effect Unit -> r -> World -> Effect Unit
onStartApp (GameM reader) game world = do
    let appHandler = {world}
    let env = GameEnv {appHandler,env: game }
    runReaderT reader env

onUpdate::World -> Effect Boolean
onUpdate world = do
    pure true

onQuit::World -> Effect Unit
onQuit world = do
    pure unit

askWorld::forall m.MonadApp m => m World
askWorld = do
    handle <- askAppHandler
    pure handle.world