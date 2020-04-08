module Seija.Simple2D where

import Prelude

import Control.Monad.Reader (ask, lift)
import Data.Array (unsafeIndex)
import Data.Vec (Vec, vec2, vec3)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Seija.App (AppReader, askWorld)
import Seija.Component (ComponentType(..), buildProp, sSize, tScale)
import Seija.Foreign (_getViewPortSize)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f, zeroVec2)

type Entity = Int

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
      _ <- F.addTransformByProp world e  $ buildProp [tScale (vec3 1.0 1.0 1.0)] Rect2D
      F.addRect2DByProp world e $ buildProp [sSize winSize] Rect2D
   _ <- addCABEventRoot e
   pure e

getViewPortSize::AppReader Vector2f
getViewPortSize = do
  world <- askWorld
  arr <- liftEffect $ _getViewPortSize world
  let x = unsafePartial $ unsafeIndex arr 0
  let y = unsafePartial $ unsafeIndex arr 1
  pure $ vec2 x y