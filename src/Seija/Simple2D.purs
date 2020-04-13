module Seija.Simple2D where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array (unsafeIndex)
import Data.Vec (vec2, vec3)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Seija.App (AppReader, askWorld)
import Seija.Component (ComponentType(..), buildProp, rSize, tScale)
import Seija.Foreign (Entity)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f)

newEntity::AppReader Entity
newEntity = do
   handle <- ask
   eid <- liftEffect $ F.newEntity handle.world
   pure eid

addCABEventRoot::Entity -> AppReader Entity
addCABEventRoot e = do
   world <- askWorld
   liftEffect $ F.addCABEventRoot world e
   pure e

newEventRoot::AppReader Entity
newEventRoot = do
   world <- askWorld
   e <- newEntity
   winSize <- getViewPortSize
   _ <- liftEffect do 
      _ <- F.addTransformByProp world e  $ buildProp  [tScale (vec3 1.0 1.0 1.0)]  Rect2D false
      F.addRect2DByProp world e $ buildProp [rSize winSize] Rect2D false
   _ <- addCABEventRoot e
   pure e

getViewPortSize::AppReader Vector2f
getViewPortSize = do
  world <- askWorld
  arr <- liftEffect $ F._getViewPortSize world
  let x = unsafePartial $ unsafeIndex arr 0
  let y = unsafePartial $ unsafeIndex arr 1
  pure $ vec2 x y