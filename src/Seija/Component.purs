module Seija.Component where

import Color (Color)
import Data.Array (filter)
import Data.ColorEx (toNumberArray)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D3)
import Data.Vec (Vec, toArray)
import Foreign.Object as O
import Prelude (class Eq, eq, map, ($))
import Seija.Math.Vector (Vector3f, Vector2f)
import Unsafe.Coerce (unsafeCoerce)

data ComponentType = Transform | Rect2D | ImageRender | SpriteRender | CABEventRoot | TextRender 
derive instance eqComponentType :: Eq ComponentType

data Prop = Prop ComponentType String PropValue

foreign import data PropValue ∷ Type

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


prop::forall a. (IsProp a) => ComponentType -> String -> a -> Prop
prop com key a = Prop com key (toPropValue a)

buildProp::(Array Prop) -> ComponentType -> O.Object PropValue
buildProp arr cType = O.fromFoldable $ map (\(Prop _ k v) -> k /\ v ) $ filter (\(Prop ct _ _) -> eq ct cType) arr

tPos:: Vector3f -> Prop
tPos = prop Transform "pos"

tScale::Vector3f -> Prop
tScale = prop Transform "scale"

tRotate::Vector3f -> Prop
tRotate = prop Transform "rotate"
    

sSize::Vector2f -> Prop
sSize = prop Rect2D "size"

sAnchor::Vector2f -> Prop
sAnchor = prop Rect2D "anchor"

iColor::Color -> Prop
iColor = prop ImageRender "color"

