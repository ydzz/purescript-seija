module Seija.FRP where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Seija.App (class MonadApp, askWorld)
import Seija.Foreign (Entity, RawBehavior, RawEvent, World, _attachBehavior, _getBehaviorValue, _mapBehavior, _mergeEvent, _newBehavior, _setBehaviorCallback, _setBehaviorFoldFunc, _tagBehavior, chainEvent, chainEventEffect, getEvent)

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

fetchEvent::forall a m.(MonadApp m) => Entity -> EventType -> Boolean -> m (Event a)
fetchEvent eid typ isCapture =  do
    world <- askWorld
    ev <- liftEffect $ getEvent world eid (numEventType typ) isCapture
    pure $ Event ev

effectEvent::forall a m.(MonadEffect m) => Event a -> (a -> Effect Unit) -> m Unit
effectEvent (Event ev) f = liftEffect $ chainEventEffect ev f

mergeEvent::forall a.Array (Event a) -> Effect (Event a)
mergeEvent events = do
  let rawEvents = map (\(Event re) -> re) events
  ev <- _mergeEvent rawEvents
  pure $ Event ev

tagBehavior::forall a b. Behavior a -> Event b -> Effect (Event a)
tagBehavior (Behavior b) (Event e) = do
  newEv <- _tagBehavior b e
  pure $ Event newEv


newtype Behavior a = Behavior RawBehavior

instance functorBehavior :: Functor Behavior where
  map f (Behavior b) = Behavior $ _mapBehavior b f

newBehavior::forall a. a -> Behavior a
newBehavior val = Behavior $ _newBehavior val

unsafeBehaviorValue::forall a.  Behavior a -> a
unsafeBehaviorValue (Behavior b) = _getBehaviorValue b

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

attachFoldBehavior::forall ea a m. (MonadEffect m) => Event ea -> Behavior a -> (a -> ea -> a) -> m Unit
attachFoldBehavior (Event ev) (Behavior b) fn = do
  liftEffect $ _attachBehavior ev b
  liftEffect $ _setBehaviorFoldFunc b fn

effectBehavior::forall a m.(MonadEffect m) => Behavior a -> (a -> Effect Unit) -> m Unit
effectBehavior (Behavior b) f = liftEffect $ _setBehaviorCallback b f

tagMapBehavior::forall ba bb e. Behavior ba -> Event e -> (ba -> bb) -> Effect (Behavior bb)
tagMapBehavior b e f = do
  ev::Event ba <- tagBehavior b e
  foldBehavior (f $ (unsafeBehaviorValue b)) ev (const f)

foldMapBehavior::forall ba bb. Behavior ba -> Event ba -> (ba -> bb) -> Effect (Behavior bb)
foldMapBehavior b e f = foldBehavior (f $ (unsafeBehaviorValue b)) e (const f)