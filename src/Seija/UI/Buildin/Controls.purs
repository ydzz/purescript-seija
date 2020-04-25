module Seija.UI.Buildin.Controls where

import Prelude

import Color (white)
import Data.Maybe (Maybe(..))
import Data.Vec (vec2)
import Effect.Class (liftEffect)
import Seija.App (class MonadApp, askWorld)
import Seija.Asset (Asset2D)
import Seija.Component as C
import Seija.Element (emptyElement, spriteB, text)
import Seija.FRP (Behavior, Event, EventType(..), fetchEvent, holdBehavior, mergeEvent)
import Seija.Foreign (Entity)

type UISkin =  { 
  defaultFont::Asset2D,
  defaultSheet::Asset2D
}

class  HasUISkin a where
  getUISkin::a -> UISkin


checkBox::forall m. (MonadApp m) => Behavior Boolean -> m Entity
checkBox b = pure 0

button::forall m.MonadApp m =>  Asset2D -> Asset2D -> String -> Array C.Prop -> Maybe Entity -> m (Event Entity)
button asset font txt props parent = do
  world <- askWorld
  root <- emptyElement props parent
  eStart <- fetchEvent root TouchStart false
  eEnd   <- fetchEvent root TouchEnd false
  mEv    <- liftEffect $ mergeEvent [eStart $> "button-active",eEnd $> "button"]
  bSpriteName <- liftEffect $ holdBehavior "button" mEv
  elSpr <- spriteB asset bSpriteName [C.rSize $ vec2 100.0 100.0,C.imageSlice0Type] (Just root)
  _ <- text font [C.tText txt,C.rSize $ vec2 100.0 100.0,C.cColor white] (Just elSpr)
  fetchEvent root Click false