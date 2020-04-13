module Seija.Component where

import Prelude

import Color (Color)
import Data.Array (filter, length)
import Data.ColorEx (toNumberArray)
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


setBehaviorWorld::ComponentType -> (World -> Entity -> O.Object PropValue -> Effect Unit) -> World -> Entity -> (Array Prop) -> Effect Unit
setBehaviorWorld ct fn world e props = do
  let tBProps = buildProp props ct true
  let len = length $ O.keys tBProps
  when (len > 0) $ fn world e tBProps  

setTransformBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setTransformBehaviorWorld = setBehaviorWorld Transform _setTransformBehavior

setRect2dBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setRect2dBehaviorWorld = setBehaviorWorld Rect2D _setRect2dBehavior