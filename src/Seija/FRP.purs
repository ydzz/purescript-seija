module Seija.FRP where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as R
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Seija.App (class MonadApp, askWorld)
import Seija.Foreign (class ToFFIJsObject, toJsObject)
import Seija.Foreign as F

newtype Event a = Event F.RawEvent

data EventType = TouchStart | TouchEnd | Click | MouseMove | MouseEnter | MouseLeave | Keyboard

data UpdateType  = Frame Int | Time Number

instance toFFIJsObjectUpdateType :: ToFFIJsObject UpdateType where
  toJsObject (Frame n) = unsafeToForeign   [0,n]
  toJsObject (Time n)  = unsafeToForeign   [unsafeToForeign 1,unsafeToForeign n]

numEventType :: EventType -> Int
numEventType TouchStart = 0
numEventType TouchEnd   = 1
numEventType Click      = 2
numEventType MouseMove  = 3
numEventType MouseEnter = 4
numEventType MouseLeave = 5
numEventType Keyboard   = 6

instance functorEvent :: Functor Event where
  map f (Event ev) = Event $ F.chainEvent ev f

fetchEventWorld::forall a. F.World -> F.Entity -> EventType -> Boolean -> Effect (Event a)
fetchEventWorld world eid typ isCapture = do
  ev <- F.getEvent world eid (numEventType typ) isCapture
  pure $ Event ev

fetchEvent::forall a m.(MonadApp m) => F.Entity -> EventType -> Boolean -> m (Event a)
fetchEvent eid typ isCapture =  do
    world <- askWorld
    ev <- liftEffect $ F.getEvent world eid (numEventType typ) isCapture
    pure $ Event ev

fetchTimeEventWorld::forall m. (MonadEffect m)=> F.World -> F.Entity -> UpdateType -> m (Event Unit)
fetchTimeEventWorld world eid updateType = do 
  ev <- liftEffect $ F._fetchTimeEvent  world eid (toJsObject updateType)
  pure $ Event ev

fetchTimeEvent::forall m.(MonadApp m) => F.Entity -> UpdateType -> m (Event Unit)
fetchTimeEvent eid updateType = askWorld >>= (\w -> fetchTimeEventWorld w eid updateType)

fetchGlobalEventWrold::forall a m. (MonadEffect m) => F.World -> F.Entity -> EventType -> m (Event a)
fetchGlobalEventWrold  world eid evType = do 
  ev <- liftEffect $ F._fetchGlobalEvent  world eid (numEventType evType)
  pure $ Event ev

fetchGlobalKeyEvent::forall m.(MonadApp m) => F.Entity -> m (Event (Tuple Int Boolean))
fetchGlobalKeyEvent eid  = askWorld >>= (\w -> fetchGlobalEventWrold w eid Keyboard)

newEvent::forall a m.(MonadEffect m) => m (Event a)
newEvent = do
  rawEv <- liftEffect $ F._newEvent
  pure $ Event rawEv

setNextEvent::forall a m.MonadEffect m => Event a -> Event a -> m Unit
setNextEvent (Event a) (Event b) = liftEffect $ F._setNextEvent a b



effectEvent::forall a m.(MonadEffect m) => Event a -> (a -> Effect Unit) -> m Unit
effectEvent (Event ev) f = liftEffect $ F.chainEventEffect ev f

mergeEvent::forall a m.MonadEffect m =>  Array (Event a) -> m (Event a)
mergeEvent events = do
  let rawEvents = map (\(Event re) -> re) events
  ev <- liftEffect $ F._mergeEvent rawEvents
  pure $ Event ev

tagBehavior::forall a b m.MonadEffect m => Behavior a -> Event b -> m (Event a)
tagBehavior (Behavior b) (Event e) = do
  newEv <- liftEffect $ F._tagBehavior b e
  pure $ Event newEv


newtype Behavior a = Behavior F.RawBehavior

instance functorBehavior :: Functor Behavior where
  map f (Behavior b) = Behavior $ F._mapBehavior b f

newBehavior::forall a. a -> Behavior a
newBehavior val = Behavior $ F._newBehavior val

unsafeBehaviorValue::forall a.  Behavior a -> a
unsafeBehaviorValue (Behavior b) = F._getBehaviorValue b

attachBehavior::forall a. Event a -> Behavior a -> Effect Unit
attachBehavior (Event ev) (Behavior b) = F._attachBehavior ev b

holdBehavior::forall a m.MonadEffect m =>  a -> Event a -> m (Behavior a)
holdBehavior val ev = do
  let b = newBehavior val
  liftEffect $ attachBehavior ev b
  pure b

foldBehavior::forall ea a m.MonadEffect m => a -> Event ea -> (a -> ea -> a) -> m (Behavior a)
foldBehavior val e@(Event re) f = do
  let b@(Behavior rb) = newBehavior val
  liftEffect $ F._attachBehavior re rb
  liftEffect $ F._setBehaviorFoldFunc rb f
  pure b

attachFoldBehavior::forall ea a m. (MonadEffect m) => Event ea -> Behavior a -> (a -> ea -> a) -> m Unit
attachFoldBehavior (Event ev) (Behavior b) fn = do
  liftEffect $ F._attachBehavior ev b
  liftEffect $ F._setBehaviorFoldFunc b fn

effectBehavior::forall a m.(MonadEffect m) => Behavior a -> (a -> Effect Unit) -> m Unit
effectBehavior (Behavior b) f = liftEffect $ F._setBehaviorCallback b f

tagMapBehavior::forall ba bb e. Behavior ba -> Event e -> (ba -> bb) -> Effect (Behavior bb)
tagMapBehavior b e f = do
  ev::Event ba <- tagBehavior b e
  foldBehavior (f $ (unsafeBehaviorValue b)) ev (const f)

foldMapBehavior::forall ba bb. Behavior ba -> Event ba -> (ba -> bb) -> Effect (Behavior bb)
foldMapBehavior b e f = foldBehavior (f $ (unsafeBehaviorValue b)) e (const f)

newtype Dynamic a = Dynamic {
  event:: Event a,
  behavior:: Behavior a
}

holdDynamic::forall a. a -> Event a -> Effect (Dynamic a)
holdDynamic val e = do
  behavior <- holdBehavior val e
  pure $ Dynamic {event : e, behavior }

foldDynamic::forall a b m.(MonadEffect m) => a -> Event b -> (a -> b -> a) -> m (Dynamic a)
foldDynamic val e f = do
  behavior <- foldBehavior val e f
  eb <- liftEffect $ tagBehavior behavior e
  pure $ Dynamic { event:eb, behavior }

current::forall a. Dynamic a -> Behavior a
current (Dynamic d) = d.behavior

updated::forall a.Dynamic a -> Event a
updated (Dynamic d) = d.event


newtype EventBox a = EventBox {
  value:: R.Ref (Maybe (Event a))
}

newEventBox:: forall m a.MonadEffect m => m (EventBox a)
newEventBox = do
  ref <- liftEffect $ R.new Nothing
  pure $ EventBox {value:ref }

putEventBox::forall a m. MonadEffect m => Event a -> EventBox a -> m Unit 
putEventBox val (EventBox box) = liftEffect $ R.write (Just val) box.value
 

unsafeUnEventBox::forall a m. MonadEffect m => EventBox a -> m (Event a)
unsafeUnEventBox (EventBox box) = do
  val <- liftEffect $ R.read box.value
  liftEffect $ R.write Nothing box.value
  pure $ unsafePartial $ fromJust val