module Seija.Element where

import Prelude

import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Seija.App (AppReader, askWorld)
import Seija.Asset (Asset2D(..), getTextureSizeWorld)
import Seija.Component (ComponentType(..), Prop, buildProp, propFromVector2f)
import Seija.Foreign (addImageRenderByProp, addRect2DByProp, addTransformByProp, newEntity, setParent)
import Seija.Simple2D (Entity)

image::Asset2D -> Array Prop -> Maybe Entity -> AppReader Entity
image s2d@(Asset2D asset) arr parent = do
    world <- askWorld
    liftEffect $ do
     e <- newEntity world
     _ <- addTransformByProp world e (buildProp arr Transform)
     case parent of
        Just p -> setParent world e p
        Nothing -> pure unit
     let rectProp =  buildProp arr Rect2D
     _ <- if (not $ O.member "size" rectProp) 
      then do
        let vecSize = getTextureSizeWorld world s2d
        let newRectProp = O.insert "size" (propFromVector2f vecSize) rectProp
        addRect2DByProp world e newRectProp
      else do
        addRect2DByProp world e rectProp
     _ <- addImageRenderByProp world e asset.assetId (buildProp arr ImageRender)
     pure e