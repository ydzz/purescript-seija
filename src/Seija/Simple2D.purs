module Seija.Simple2D where

import Prelude

import Data.Array (unsafeIndex)
import Data.Vec (vec2, vec3)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Seija.App (class MonadApp, askAppHandler, askWorld)
import Seija.Component as C
import Seija.Foreign (Entity)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f)

newEntity::forall m.MonadApp m => m Entity
newEntity = do
  app <- askAppHandler
  liftEffect $ F.newEntity app.world

childrens::forall m.MonadApp m => Entity -> m (Array Entity)
childrens p = do
   world <- askWorld
   liftEffect $ F.getChildrens world p

addCABEventRoot::forall m.MonadApp m => Entity -> m Entity
addCABEventRoot e = do
   app <- askAppHandler
   liftEffect $ F.addCABEventRoot app.world e
   pure e

newEventRoot::forall m.MonadApp m => m Entity
newEventRoot = do
   world <- askWorld
   e <- newEntity
   winSize <- getViewPortSize
   _ <- liftEffect do 
      _ <- F.addTransformByProp world e  $ C.buildProp  [C.tScale (vec3 1.0 1.0 1.0)]  C.Rect2D false
      F.addRect2DByProp world e $ C.buildProp [C.rSize winSize] C.Rect2D false
   _ <- addCABEventRoot e
   pure e


getViewPortSize::forall m.MonadApp m => m Vector2f
getViewPortSize = do
  world <- askWorld
  arr <- liftEffect $ F._getViewPortSize world
  let x = unsafePartial $ unsafeIndex arr 0
  let y = unsafePartial $ unsafeIndex arr 1
  pure $ vec2 x y