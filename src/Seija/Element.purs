module Seija.Element (
  image,sprite,text
) where

import Prelude

import Data.Array (index)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Seija.App (AppReader, askWorld)
import Seija.Asset (Asset2D(..), getSpirteRectInfo, getTextureSizeWorld)
import Seija.Component (ComponentType(..), Prop, buildProp, isImageTypeDefSize, propFromVector2f, setRect2dBehaviorWorld, setTransformBehaviorWorld)
import Seija.Foreign (Entity, World, _addSpriteRenderByProp, _addTextRenderByProp, _addTransparent, addImageRenderByProp, addRect2DByProp, addTransformByProp, newEntity, setParent)
import Unsafe.Coerce (unsafeCoerce)


image::Asset2D -> Array Prop -> Maybe Entity -> AppReader Entity
image s2d@(Asset2D asset) arr parent = do
    world <- askWorld
    liftEffect $ do
     e <- newEntity world
     _ <- addTransformByProp world e (buildProp arr Transform false)
     addParent world e parent
     let rectProp =  buildProp arr Rect2D false
     if (not $ O.member "size" rectProp) 
      then do
        let vecSize = getTextureSizeWorld world s2d
        let newRectProp = O.insert "size" (propFromVector2f vecSize) rectProp
        addRect2DByProp world e newRectProp *> pure unit
      else  addRect2DByProp world e rectProp *> pure unit
     _ <- addImageRenderByProp world e asset.assetId (buildProp arr ImageRender false)
     --set behavior
     setTransformBehaviorWorld world e arr
     setRect2dBehaviorWorld world e arr
     pure e

sprite::Asset2D -> String -> Array Prop -> Maybe Entity -> AppReader Entity
sprite s2d@(Asset2D asset) spriteName arr parent = do
  world <- askWorld
  liftEffect $ do
    e <- newEntity world
    _ <- addTransformByProp world e (buildProp arr Transform false)
    addParent world e parent
    let rectProp =  buildProp arr Rect2D false
    let isNoSize = not $ O.member "size" rectProp
    if (isNoSize && isSetDefault mayImageIntType) 
    then do
        let (_ /\ _ /\ w /\ h /\ unit) = getSpirteRectInfo world s2d spriteName
        let newRectProp = O.insert "size" (propFromVector2f $ vec2 w h) rectProp
        addRect2DByProp world e newRectProp *> pure unit
    else  addRect2DByProp world e rectProp *> pure unit
    _addSpriteRenderByProp world e asset.assetId spriteName spriteProp
    _addTransparent world e
    --set behavior
    setTransformBehaviorWorld world e arr
    setRect2dBehaviorWorld world e arr
    pure e
  where
    spriteProp = buildProp arr SpriteRender false
    mayImageIntType::Maybe Int
    mayImageIntType = do
      typeProp <- O.lookup "type" spriteProp
      (fistNumber::Number) <- index (unsafeCoerce typeProp) 0
      pure $ unsafePartial $ fromJust $ fromNumber fistNumber
    isSetDefault::Maybe Int -> Boolean
    isSetDefault Nothing = true
    isSetDefault (Just v) = isImageTypeDefSize v

text::Asset2D -> Array Prop -> Maybe Entity -> AppReader Entity
text s2d@(Asset2D asset) arr parent = do
 world <- askWorld
 liftEffect $ do
    e <- newEntity world
    _ <- addTransformByProp world e (buildProp arr Transform false)
    let rectProp =  buildProp arr Rect2D false
    _ <- addRect2DByProp world e rectProp
    addParent world e parent
    _addTransparent world e
    _addTextRenderByProp world e asset.assetId $ buildProp arr TextRender false
    pure e


addParent::World -> Entity -> Maybe Entity -> Effect Unit
addParent _ _ Nothing = pure unit
addParent world e (Just p) = setParent world e p