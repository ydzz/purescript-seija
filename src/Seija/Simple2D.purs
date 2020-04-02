module Seija.Simple2D where

import Prelude

import Control.Monad.Reader (ask)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Seija.App (AppReader, askWorld)
import Seija.Asset (Asset2D(..))
import Seija.Foreign (toForeign)
import Seija.Foreign as F

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
    

newImage::Asset2D -> Maybe Entity -> AppReader Entity
newImage (Asset2D s2d) parent = do
   world <- askWorld
   let jsParent = fromMaybe (toForeign false) (map toForeign parent)
   liftEffect $ F._newImage world s2d.resId jsParent (O.empty)