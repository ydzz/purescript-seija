module Seija.UI.Buildin.Controls where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as R
import Seija.App (class MonadApp, GameM, askEnv)
import Seija.Asset (Asset2D, fontPath, loadAssetSync, spriteSheetPath)
import Seija.Element (sprite_)
import Seija.Foreign (Entity)

type UISkin =  { 
  defaultFont::Asset2D,
  defaultSheet::Asset2D,
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
  sheet <- loadAssetSync (spriteSheetPath "material.json")
  font <- loadAssetSync (fontPath "WenQuanYiMicroHei.ttf")
  writeSkin { defaultSheet:sheet, defaultFont:font ,fontSize:20 }

--unsafeAskUISkin::forall m. (MonadSkin m) => m UISkin
--unsafeAskUISkin = do
--  skin <- askSkin
--  undefined

--checkBox::forall m. MonadApp m => MonadSkin m => m Entity
--checkBox = sprite_

{-
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
-}