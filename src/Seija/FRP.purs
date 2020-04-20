module Seija.FRP where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Seija.App (AppReader, askWorld)
import Seija.Foreign (Entity, RawBehavior, RawEvent, World, _attachBehavior, _mergeEvent, _newBehavior, _setBehaviorCallback, _setBehaviorFoldFunc, chainEvent, chainEventEffect, getEvent)

newtype Event a = Event RawEvent

data EventType = TouchStart | TouchEnd | Click | MouseMove | MouseEnter | MouseLeave

numEventType :: EventType -> Int
numEventType TouchStart = 0
numEventType TouchEnd   = 1
numEventType Click      = 2
numEventType MouseMove  = 3
numEventType MouseEnter = 4
numEventType MouseLeave = 5

instance functorEvent :: Functor Event where
  map f (Event ev) = Event $ chainEvent ev f

fetchEventWorld::forall a. World -> Entity -> EventType -> Boolean -> Effect (Event a)
fetchEventWorld world eid typ isCapture = do
  ev <- getEvent world eid (numEventType typ) isCapture
  pure $ Event ev

fetchEvent::forall a.Entity -> EventType -> Boolean -> AppReader (Event a)
fetchEvent eid typ isCapture =  do
    world <- askWorld
    ev <- liftEffect $ getEvent world eid (numEventType typ) isCapture
    pure $ Event ev

effectEvent::forall a. Event a -> (a -> Effect Unit) -> Effect Unit
effectEvent (Event ev) f = chainEventEffect ev f


newtype Behavior a = Behavior RawBehavior

newBehavior::forall a. a -> Behavior a
newBehavior val = Behavior $ _newBehavior val

attachBehavior::forall a. Event a -> Behavior a -> Effect Unit
attachBehavior (Event ev) (Behavior b) = _attachBehavior ev b

holdBehavior::forall a.a -> Event a -> Effect (Behavior a)
holdBehavior val ev = do
  let b = newBehavior val
  attachBehavior ev b
  pure b

foldBehavior::forall ea a.a -> Event ea -> (a -> ea -> a) -> Effect (Behavior a)
foldBehavior val e@(Event re) f = do
  let b@(Behavior rb) = newBehavior val
  _attachBehavior re rb
  _setBehaviorFoldFunc rb f
  pure b

attachFoldBehavior::forall ea a. Event ea -> Behavior a -> (a -> ea -> a) -> Effect Unit
attachFoldBehavior (Event ev) (Behavior b) fn = do
  _attachBehavior ev b
  _setBehaviorFoldFunc b fn
  

effectBehavior::forall a.Behavior a -> (a -> Effect Unit) -> Effect Unit
effectBehavior (Behavior b) f = _setBehaviorCallback b f

mergeEvent::forall a.Array (Event a) -> Effect (Event a)
mergeEvent events = do
  let rawEvents = map (\(Event re) -> re) events
  ev <- _mergeEvent rawEvents
  pure $ Event ev