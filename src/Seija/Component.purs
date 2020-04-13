module Seija.Component where

import Prelude

import Color (Color)
import Data.Array (filter, length)
import Data.ColorEx (toNumberArray)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D3)
import Data.Vec (Vec, toArray)
import Effect (Effect)
import Foreign.Object as O
import Seija.FRP (Behavior)
import Seija.Foreign (Entity, PropValue, World, _setRect2dBehavior, _setTransformBehavior)
import Seija.Math.Vector (Vector3f, Vector2f)
import Unsafe.Coerce (unsafeCoerce)

data ComponentType = Transform | Rect2D | ImageRender | SpriteRender | CABEventRoot | TextRender 
derive instance eqComponentType :: Eq ComponentType


data ImageType = Simple | Slice Number Number Number Number | SheetSlice Int | Filled ImageFilledType Number | Tiled
data ImageFilledType = HorizontalLeft | HorizontalRight | VerticalTop | VerticalBottom

isImageTypeDefSize::Int -> Boolean
isImageTypeDefSize 0 = true
isImageTypeDefSize 1 = false
isImageTypeDefSize 2 = false
isImageTypeDefSize 3 = true
isImageTypeDefSize 4 = false
isImageTypeDefSize _ = false

imageTypeToInt::ImageType -> Int
imageTypeToInt Simple = 0
imageTypeToInt (Slice _ _ _ _) = 1
imageTypeToInt (SheetSlice _) = 2
imageTypeToInt (Filled _ _) = 3
imageTypeToInt Tiled = 4

imageFilledTypeToInt::ImageFilledType -> Int
imageFilledTypeToInt HorizontalLeft   = 0
imageFilledTypeToInt HorizontalRight  = 0
imageFilledTypeToInt VerticalTop      = 0
imageFilledTypeToInt VerticalBottom   = 0

data Prop = Prop ComponentType Boolean String PropValue



propFromString ∷ String -> PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean -> PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int -> PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number -> PropValue
propFromNumber = unsafeCoerce

propFromVector3f ∷ Vector3f -> PropValue
propFromVector3f v = unsafeCoerce $ toArray v

propFromVector2f ∷ Vector2f -> PropValue
propFromVector2f v = unsafeCoerce $ toArray v

propFromColor::Color -> PropValue
propFromColor c = unsafeCoerce $ toNumberArray c

propFromImageType ∷ ImageType -> PropValue
propFromImageType Simple                = unsafeCoerce $ [0]
propFromImageType (Slice _0 _1 _2 _3)   = unsafeCoerce $ [1.0,_0,_1,_2,_3]
propFromImageType (SheetSlice _0)       = unsafeCoerce $ [2,_0]
propFromImageType (Filled _0 _1)        = unsafeCoerce $ [3.0,toNumber $ imageFilledTypeToInt _0,_1]
propFromImageType Tiled                 = unsafeCoerce $ [4]

propFromBehavior::forall a. Behavior a -> PropValue
propFromBehavior = unsafeCoerce

class IsProp a where
  toPropValue :: a -> PropValue

instance isPropString :: IsProp String where
  toPropValue = propFromString

instance isPropInt :: IsProp Int where
  toPropValue = propFromInt

instance isPropNumber :: IsProp Number where
  toPropValue = propFromNumber

instance isPropVector3f :: IsProp (Vec D3 Number) where
  toPropValue = propFromVector3f

instance isPropVector2f :: IsProp (Vec D2 Number) where
  toPropValue = propFromVector2f

instance isPropColor :: IsProp Color where
  toPropValue = propFromColor

instance isImageType :: IsProp ImageType where
  toPropValue = propFromImageType

instance isPropBehavior :: IsProp (Behavior a) where
  toPropValue = propFromBehavior


prop::forall a. (IsProp a) => ComponentType -> Boolean -> String -> a -> Prop
prop com b key a = Prop com b key (toPropValue a)

buildProp::(Array Prop) -> ComponentType -> Boolean -> O.Object PropValue
buildProp arr cType isBehavior = O.fromFoldable $ map (\(Prop _ _ k v) -> k /\ v ) fList
 where
   fList = filter (\(Prop ct isb _ _) -> (ct == cType) && (isBehavior == isb)) arr

tPos:: Vector3f -> Prop
tPos = prop Transform false "pos"

tPosB::Behavior Vector3f -> Prop
tPosB = prop Transform true "pos"

tScale::Vector3f -> Prop
tScale = prop  Transform false  "scale"

tRotate::Vector3f -> Prop
tRotate = prop Transform false "rotate"
    

rSize::Vector2f -> Prop
rSize = prop  Rect2D false "size"

rSizeB::Behavior Vector2f -> Prop
rSizeB = prop  Rect2D true "size"

rAnchor::Vector2f -> Prop
rAnchor = prop Rect2D false "anchor"

iColor::Color -> Prop
iColor = prop ImageRender false "color"

iColorB::Behavior Color -> Prop
iColorB = prop ImageRender true "color"

imageType::ImageType -> Prop
imageType = prop SpriteRender false "type"

imageSlice0Type::Prop
imageSlice0Type = imageType $ SheetSlice 0


setBehaviorWorld::ComponentType -> (World -> Entity -> O.Object PropValue -> Effect Unit) -> World -> Entity -> (Array Prop) -> Effect Unit
setBehaviorWorld ct fn world e props = do
  let tBProps = buildProp props ct true
  let len = length $ O.keys tBProps
  when (len > 0) $ fn world e tBProps  

setTransformBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setTransformBehaviorWorld = setBehaviorWorld Transform _setTransformBehavior

setRect2dBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setRect2dBehaviorWorld = setBehaviorWorld Rect2D _setRect2dBehavior