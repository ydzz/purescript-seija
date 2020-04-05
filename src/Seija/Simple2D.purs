module Seija.Simple2D where

import Prelude

import Control.Monad.Reader (ask)
import Color (Color)
import Data.ColorEx (toNumberArray)
import Data.Maybe (Maybe, fromMaybe)
import Data.Vec (toArray, vec2)
import Effect.Class (liftEffect)
import Seija.App (AppReader, askWorld)
import Seija.Asset (Asset2D(..), asset2dId)
import Seija.Foreign (toForeign)
import Seija.Foreign as F
import Seija.Math.Vector (Vector3f, Vector2f)

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
newEventRoot = newEntity >>= addCABEventRoot