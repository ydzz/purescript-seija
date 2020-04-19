module Seija.Element (
  image,sprite,text,spriteB,sprite_
) where

import Prelude

import Data.Array (index)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.MaybeEx (maybeToList)
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Seija.App (AppReader, askWorld)
import Seija.Asset (Asset2D(..), getSpirteRectInfo, getTextureSizeWorld)
import Seija.Component (ComponentType(..), POrB(..), Prop, buildProp, isImageTypeDefSize, propFromVector2f, propPOrB, setRect2dBehaviorWorld, setSpriteBehaviorWorld, setTransformBehaviorWorld, spriteNameB, valPOrB)
import Seija.FRP (Behavior)
import Seija.Foreign as F
import Unsafe.Coerce (unsafeCoerce)


image::Asset2D -> Array Prop -> Maybe F.Entity -> AppReader F.Entity
image s2d@(Asset2D asset) arr parent = do
    world <- askWorld
    liftEffect $ do
     e <- F.newEntity world
     _ <- F.addTransformByProp world e (buildProp arr Transform false)
     addParent world e parent
     let rectProp =  buildProp arr Rect2D false
     if (not $ O.member "size" rectProp)
       then do
        let vecSize = getTextureSizeWorld world s2d
        let newRectProp = O.insert "size" (propFromVector2f vecSize) rectProp
        F.addRect2DByProp world e newRectProp *> pure unit
      else  F.addRect2DByProp world e rectProp *> pure unit
     _ <- F.addImageRenderByProp world e asset.assetId (buildProp arr ImageRender false)
     --set behavior
     setTransformBehaviorWorld world e arr
     setRect2dBehaviorWorld world e arr
     pure e

spriteB::Asset2D -> Behavior String ->  Array Prop -> Maybe F.Entity -> AppReader F.Entity
spriteB s2d b = sprite s2d (B b)

sprite_::Asset2D -> String ->  Array Prop -> Maybe F.Entity -> AppReader F.Entity
sprite_ s2d p = sprite s2d (P p)

sprite::Asset2D -> POrB String -> Array Prop -> Maybe F.Entity -> AppReader F.Entity
sprite s2d@(Asset2D asset) spr arr parent = do
  let spriteName = valPOrB spr
  let spritePropArr = maybeToList $ propPOrB spr spriteNameB
  world <- askWorld
  liftEffect $ do
    e <- F.newEntity world
    _ <- F.addTransformByProp world e (buildProp arr Transform false)
    addParent world e parent
    let rectProp =  buildProp arr Rect2D false
    let isNoSize = not $ O.member "size" rectProp
    if (isNoSize && isSetDefault mayImageIntType) 
    then do
        let (_ /\ _ /\ w /\ h /\ unit) = getSpirteRectInfo world s2d spriteName
        let newRectProp = O.insert "size" (propFromVector2f $ vec2 w h) rectProp
        F.addRect2DByProp world e newRectProp *> pure unit
    else  F.addRect2DByProp world e rectProp *> pure unit
    F._addSpriteRenderByProp world e asset.assetId spriteName spriteProp
    F._addTransparent world e
    --set behavior
    setTransformBehaviorWorld world e arr
    setRect2dBehaviorWorld world e arr
    setSpriteBehaviorWorld world e (arr <> spritePropArr)
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

text::Asset2D -> Array Prop -> Maybe F.Entity -> AppReader F.Entity
text s2d@(Asset2D asset) arr parent = do
 world <- askWorld
 liftEffect $ do
    e <- F.newEntity world
    _ <- F.addTransformByProp world e (buildProp arr Transform false)
    let rectProp =  buildProp arr Rect2D false
    _ <- F.addRect2DByProp world e rectProp
    addParent world e parent
    F._addTransparent world e
    F._addTextRenderByProp world e asset.assetId $ buildProp arr TextRender false
    pure e


addParent::F.World -> F.Entity -> Maybe F.Entity -> Effect Unit
addParent _ _ Nothing = pure unit
addParent world e (Just p) = F.setParent world e p
