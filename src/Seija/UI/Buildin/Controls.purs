module Seija.UI.Buildin.Controls where

import Prelude

import Color (white)
import Data.Array (filter, length)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as R
import Partial.Unsafe (unsafePartial)
import Seija.App (class MonadApp, GameM, askEnv)
import Seija.Asset (loadAsset)
import Seija.Asset.LoaderInfo (fontLoaderInfo, spriteSheetLoaderInfo)
import Seija.Asset.Types (Font, SpriteSheet)
import Seija.Component (Prop(..), hasPropByName)
import Seija.Component as C
import Seija.Element (emptyElement, spriteB, text)
import Seija.FRP (Dynamic(..), Event, EventType(..), fetchEvent, foldBehavior, foldMapBehavior, holdBehavior, mergeEvent, tagBehavior)
import Seija.Foreign (Entity)



type UISkin =  { 
  defaultFont::Font,
  defaultSheet::SpriteSheet,
  fontSize::Int
}

class  HasUISkin a where
   askSkinRef::a -> (R.Ref (Maybe UISkin))

class (MonadApp m) <= MonadSkin m where
 askSkin::m (Maybe UISkin)
 writeSkin::UISkin -> m Unit

instance monadSkinGameM ::(Monad m,MonadEffect m,HasUISkin r) => MonadSkin (GameM r m) where
  askSkin = do
   env <- askEnv
   let ref = askSkinRef env
   liftEffect $ R.read ref
  writeSkin skin = do
    env <- askEnv
    let ref = askSkinRef env
    liftEffect $ R.write (Just skin) ref

loadSkin::forall m.MonadApp m => MonadSkin m => m Unit
loadSkin = do
  sheet <- loadAsset (spriteSheetLoaderInfo "material.json" Nothing)
  font <-  loadAsset (fontLoaderInfo "WenQuanYiMicroHei.ttf")
  writeSkin { defaultSheet:sheet, defaultFont:font ,fontSize:16 }

unsafeAskUISkin::forall m. MonadSkin m => MonadEffect m => m UISkin
unsafeAskUISkin = do
  maySkin <- askSkin
  pure $ unsafePartial $ fromJust maySkin

checkBox::forall m. MonadApp m => MonadSkin m =>  Boolean -> Array Prop -> Maybe Entity -> m (Tuple (Event Boolean) Entity)
checkBox b prop p = do
  skin <- unsafeAskUISkin
  let arr = filter (\(Prop _ _ attrName _) -> attrName == "size") prop
  let newProps = if ((length arr) == 0) then (prop <> [C.rSize $ vec2 24.0 24.0]) else prop
  e <- emptyElement newProps p
  eClick <- fetchEvent e Click false
  bCheck <- liftEffect $ foldBehavior b eClick  (\a ea -> not a)
  evB <- liftEffect $  tagBehavior bCheck eClick
  bStr <- liftEffect $ foldMapBehavior bCheck evB (\ea -> if ea then "checkbox-checked"  else "checkbox-unchecked")
  _ <- spriteB skin.defaultSheet bStr newProps p
  pure (evB /\ e)

button::forall m. MonadApp m => MonadSkin m => String -> Array Prop -> Maybe Entity -> m  (Tuple (Event Entity) Entity)
button txt prop parent = do
  skin <- unsafeAskUISkin
  let arr = filter (\(Prop _ _ attrName _) -> attrName == "size") prop
  let sizeProp = if ((length arr) == 0) then ([C.rSize $ vec2 80.0 30.0]) else arr
  let fontSizeArr = filter (\(Prop _ _ attrName _) -> attrName == "fontSize") prop
  let fontSize = if  ((length fontSizeArr) == 0) then [C.tFontSize 24] else fontSizeArr
  empty   <- emptyElement (sizeProp <> prop) parent
  (eStart::Event Entity)  <- fetchEvent empty TouchStart false
  (eEnd::Event Entity)    <- fetchEvent empty TouchEnd false
  mEv     <- liftEffect $ mergeEvent [eStart $> "button-active",eEnd $> "button"]
  bSpriteName <- liftEffect $ holdBehavior "button" mEv
  elSpr <- spriteB skin.defaultSheet bSpriteName (sizeProp <> [C.imageSlice0Type]) (Just empty)
  _ <- text skin.defaultFont  (sizeProp <> [C.tText txt,C.cColor white] <> fontSize) (Just elSpr)
  evClick <- fetchEvent empty Click false
  pure $ evClick /\ empty

checkBox2::forall m.(MonadApp m) => MonadSkin m => Dynamic Boolean -> Array Prop -> Maybe Entity  -> m (Tuple (Event Entity) Entity)
checkBox2 (Dynamic d) prop parent = do
  skin <- unsafeAskUISkin
  let newProp = if (hasPropByName prop "size") then (prop <> [C.rSize $ vec2 24.0 24.0]) else prop
  empty <- emptyElement newProp parent
  eClick <- fetchEvent empty Click false
  bStr <- foldBehavior "checkbox-unchecked" d.event (\a ea -> if ea then "checkbox-checked" else "checkbox-unchecked")
  eSpr <- spriteB skin.defaultSheet bStr newProp (Just empty)
  pure (eClick /\ eSpr)