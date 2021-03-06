module Seija.Component where

import Prelude

import Color (Color)
import Data.Array (filter, find, length)
import Data.ColorEx (toNumberArray)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D3)
import Data.Vec (Vec, toArray, vec2, vec3)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Foreign.Object as O
import Seija.App (class MonadApp, askWorld)
import Seija.FRP (Behavior(..))
import Seija.Foreign (class ToFFIJsObject, Entity, PropValue, World, _getBehaviorValue, _setRect2dBehavior, _setSpriteRenderBehavior, _setTextRenderBehavior, _setTransformBehavior, toJsObject)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f, Vector3f, Vector4f)
import Unsafe.Coerce (unsafeCoerce)

data ComponentType = Transform | Rect2D | ImageRender | SpriteRender | CABEventRoot | TextRender | Common
derive instance eqComponentType :: Eq ComponentType


data ImageType = Simple | Slice Number Number Number Number | SheetSlice Int | Filled ImageFilledType Number | Tiled
data ImageFilledType = HorizontalLeft | HorizontalRight | VerticalTop | VerticalBottom

data POrB a = P a | B (Behavior a)

valPOrB:: forall a.POrB a -> a
valPOrB (P a) = a
valPOrB (B (Behavior b)) = _getBehaviorValue b

propPOrB::forall a.POrB a -> (Behavior a -> Prop) -> Maybe Prop
propPOrB (P _) _ = Nothing 
propPOrB (B b) f = Just $ f b 

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
   fList = filter (\(Prop ct isb _ _) -> (ct == cType || ct ==  Common) && (isBehavior == isb)) arr

tPos:: Vector3f -> Prop
tPos = prop Transform false "pos"

tPosVec3::Number -> Number -> Number -> Prop
tPosVec3 x y z = tPos $ vec3 x y z

tPosB::Behavior Vector3f -> Prop
tPosB = prop Transform true "pos"

tScale::Vector3f -> Prop
tScale = prop  Transform false  "scale"

tScaleVec3::Number -> Number -> Number -> Prop
tScaleVec3 x y z = tScale $ vec3 x y z

tRotate::Vector3f -> Prop
tRotate = prop Transform false "rotate"

rSize::Vector2f -> Prop
rSize = prop  Rect2D false "size"

rSizeVec2::Number -> Number -> Prop
rSizeVec2 x y = rSize $ vec2 x y

rSizeB::Behavior Vector2f -> Prop
rSizeB = prop  Rect2D true "size"

rAnchor::Vector2f -> Prop
rAnchor = prop Rect2D false "anchor"

rAnchorVec2::Number -> Number -> Prop
rAnchorVec2 x y = rAnchor $ vec2 x y

cColor::Color -> Prop
cColor = prop Common false "color"

cColorB::Behavior Color -> Prop
cColorB = prop Common true "color"

imageType::ImageType -> Prop
imageType = prop SpriteRender false "type"

imageSlice0Type::Prop
imageSlice0Type = imageType $ SheetSlice 0

tText::String -> Prop
tText = prop TextRender false "text"

tFontSize::Int -> Prop
tFontSize = prop TextRender false "fontSize"

tTextB::Behavior String -> Prop
tTextB = prop TextRender true "text"

tColor::Color -> Prop
tColor = prop TextRender false "color"

spriteNameB::Behavior String -> Prop
spriteNameB = prop SpriteRender true "spriteName"


setBehaviorWorld::ComponentType -> (World -> Entity -> O.Object PropValue -> Effect Unit) -> World -> Entity -> (Array Prop) -> Effect Unit
setBehaviorWorld ct fn world e props = do
  let tBProps = buildProp props ct true
  let len = length $ O.keys tBProps
  when (len > 0) $ fn world e tBProps  

setTransformBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setTransformBehaviorWorld = setBehaviorWorld Transform _setTransformBehavior

setRect2dBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setRect2dBehaviorWorld = setBehaviorWorld Rect2D _setRect2dBehavior

setSpriteBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setSpriteBehaviorWorld = setBehaviorWorld SpriteRender _setSpriteRenderBehavior

setTextBehaviorWorld::World -> Entity -> (Array Prop) -> Effect Unit
setTextBehaviorWorld = setBehaviorWorld TextRender _setTextRenderBehavior


hasProp::Array Prop -> (Prop -> Boolean) -> Boolean
hasProp arr f = isJust $ find f arr

hasPropByName :: Array Prop -> String -> Boolean
hasPropByName arr attrName = hasProp arr (\(Prop _ _ name _) -> name == "size")


data ScreenScale = ScaleWithWidth Number | ScaleWithHeight Number

addSreenScaler::forall m.(MonadApp m) => Entity -> ScreenScale -> m Boolean
addSreenScaler entity typ = do
  world <- askWorld
  case typ of
   ScaleWithWidth  num -> liftEffect $ F._addScreenScaler world entity 0 num
   ScaleWithHeight num -> liftEffect $ F._addScreenScaler world entity 1 num

data HorizontalAlign = LHLeft | LHCenter | LHRight | LHStretch

data VerticalAlign = LVTop | LVCenter | LVBottom | LVStretch

instance toFFIJsHorizontalAlign :: ToFFIJsObject HorizontalAlign where
  toJsObject LHLeft     = unsafeToForeign 0
  toJsObject LHCenter   = unsafeToForeign 1
  toJsObject LHRight    = unsafeToForeign 2
  toJsObject LHStretch  = unsafeToForeign 3

instance toFFIJsVerticalAlign :: ToFFIJsObject VerticalAlign where
  toJsObject LVTop      = unsafeToForeign 0
  toJsObject LVCenter   = unsafeToForeign 1
  toJsObject LVBottom   = unsafeToForeign 2
  toJsObject LVStretch  = unsafeToForeign 3

addBaseLayout::forall m.MonadApp m => Entity -> HorizontalAlign -> VerticalAlign -> Maybe Vector4f -> Maybe Vector4f -> m Boolean
addBaseLayout eid hor ver mayMargin mayPadding = do
  world <- askWorld
  let margin  = maybe (unsafeToForeign 0) (unsafeToForeign <<< toArray) mayMargin
  let padding = maybe (unsafeToForeign 0) (unsafeToForeign <<< toArray)  mayPadding
  liftEffect $ F._addBaseLayout world eid (toJsObject hor) (toJsObject ver) margin padding

data Orientation = Horizontal | Vertical

instance toFFIJsOrientation :: ToFFIJsObject Orientation where
  toJsObject Horizontal = unsafeToForeign 0
  toJsObject Vertical   = unsafeToForeign 1

addStackPanel::forall m.MonadApp m => Entity -> Orientation -> Number -> m Boolean
addStackPanel eid ori spec = do
  world <- askWorld
  liftEffect $ F._addStackPanel world eid (toJsObject ori) spec