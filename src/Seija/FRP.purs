module Seija.FRP where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Seija.App (AppReader, askWorld)
import Seija.Foreign (RawEvent, World, chainEvent, chainEventEffect, getEvent)
import Seija.Simple2D (Entity)

newtype Event a = Event RawEvent

instance functorEvent :: Functor Event where
  map f (Event ev) = Event $ chainEvent ev f

fetchEventWorld::forall a. World -> Entity -> Boolean -> Effect (Event a)
fetchEventWorld world eid isCapture = do
  ev <- getEvent world eid 0 isCapture
  pure $ Event ev

fetchEvent::forall a.Entity -> Boolean -> AppReader (Event a)
fetchEvent eid isCapture =  do
    world <- askWorld
    ev <- liftEffect $ getEvent world eid 0 isCapture
    pure $ Event ev

effectEvent::forall a. Event a -> (a -> Effect Unit) -> Effect Unit
effectEvent (Event ev) f = chainEventEffect ev f
