module Seija.Foreign  where


import Color (Color)
import Color.Scheme.X11 (darkgray)
import Data.ColorEx (toNumberArray)
import Data.Default (class Default, default)
import Data.Either (Either(..))
import Data.Lens (Prism', lens, prism, prism')
import Data.Maybe (Maybe(..))
import Data.MaybeEx (maybeToList)
import Data.Monoid ((<>))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as FO
import Prelude (Unit, map, ($), (<<<))
import Unsafe.Coerce (unsafeCoerce)

class ToJsObject a where
    toJsObject::a -> FO.Object Foreign
  
type WindowConfigRecord = {
    width::Int,
    height::Int,
    bgColor:: Maybe Color
}

newtype WindowConfig = WindowConfig WindowConfigRecord
derive instance newtypeWindowConfig :: Newtype WindowConfig _


instance defaultWindowConfig :: Default WindowConfig where
    default::WindowConfig
    default = WindowConfig {
        width : 1024,
        height: 768,
        bgColor: Just darkgray
    }

toForeign:: forall a. a -> Foreign
toForeign = unsafeToForeign


instance toJsOjectWindowConfig :: ToJsObject WindowConfig where
    toJsObject (WindowConfig cfg) = arr
      where
        arr = FO.fromFoldable $ ["width"  /\ toForeign cfg.width,
                                 "height" /\ toForeign cfg.height] <> color
        color::Array (Tuple String Foreign)
        color = maybeToList $ map  ((Tuple "bgColor") <<< toForeign <<< toNumberArray) cfg.bgColor


newtype Simple2dConfig = Simple2dConfig {
    window::WindowConfig
}
derive instance newtypeSimple2dConfig :: Newtype Simple2dConfig _

instance defaultSimple2dConfig :: Default Simple2dConfig where
    default::Simple2dConfig
    default = Simple2dConfig {
        window : default
    }

instance toJsObejctSimple2dConfig:: ToJsObject Simple2dConfig where
    toJsObject (Simple2dConfig cfg) = FO.fromFoldable ["window" /\ (toForeign $ toJsObject cfg.window)]

--lWindow::Prism' Simple2dConfig WindowConfig
--lWindow = prism (\w -> Simple2dConfig {window : w}) (\s -> Right $ (unwrap s).window)

_window :: forall a. Strong a => a WindowConfig WindowConfig -> a Simple2dConfig Simple2dConfig
_window = lens (\(Simple2dConfig {window}) -> window) (\(Simple2dConfig r) value -> Simple2dConfig (r {window = value}))

_bgColor :: forall a. Strong a => a (Maybe Color) (Maybe Color) -> a WindowConfig WindowConfig
_bgColor = lens (\(WindowConfig r) -> r.bgColor) (\(WindowConfig r) value -> WindowConfig (r {bgColor = value}))


_width :: forall a. Strong a => a Int Int -> a WindowConfig WindowConfig
_width = lens (\(WindowConfig r) -> r.width) (\(WindowConfig r) value -> WindowConfig (r {width = value}))

_height :: forall a. Strong a => a Int Int -> a WindowConfig WindowConfig
_height = lens (\(WindowConfig r) -> r.height) (\(WindowConfig r) value -> WindowConfig (r {height = value}))

_windowBgColor :: forall a. Strong a => a (Maybe Color) (Maybe Color) -> a Simple2dConfig Simple2dConfig
_windowBgColor =  _window <<< _bgColor


_windowWidth :: forall a. Strong a => a Int Int -> a Simple2dConfig Simple2dConfig
_windowWidth =  _window <<< _width

_windowHeight :: forall a. Strong a => a Int Int -> a Simple2dConfig Simple2dConfig
_windowHeight =  _window <<< _height

newtype AppConfig = AppConfig {
    onStart::  (World -> Effect Unit),
    onUpdate:: (World -> Effect Boolean),
    onQuit::   (World -> Effect Unit),
    resPath::Maybe String
}

instance toJsObjectAppConfig :: ToJsObject AppConfig where
    toJsObject (AppConfig cfg) = FO.fromFoldable $ ["OnStart"  /\  (toForeign  cfg.onStart),
                                                    "OnUpdate" /\  (toForeign  cfg.onUpdate),
                                                    "OnQuit"   /\  (toForeign cfg.onQuit)] <> path
     where
       path::Array (Tuple String Foreign)
       path = map (\s -> Tuple "ResPath" $ toForeign s) $ maybeToList cfg.resPath


foreign import appVersion::String
foreign import data Simple2d:: Type
foreign import data App:: Type
foreign import data World:: Type
foreign import data Loader:: Type
foreign import data RawEvent:: Type
foreign import data RawBehavior:: Type
foreign import data PropValue ∷ Type
type Entity = Int

foreign import _newSimple2d::FO.Object Foreign -> Simple2d

newSimple2d::Simple2dConfig -> Simple2d
newSimple2d cfg = _newSimple2d (toJsObject cfg)

newApp::Simple2d -> AppConfig -> App
newApp s2d cfg = _newApp s2d (toJsObject cfg)

foreign import _newApp::Simple2d -> FO.Object Foreign -> App

foreign import runApp::App -> Effect Unit

foreign import fetchLoader::World -> Effect Loader

foreign import loadAssetSync::World -> Loader -> Int -> String -> Effect Int

foreign import newEntity::World -> Effect Int

foreign import addCABEventRoot::World -> Int -> Effect Unit

foreign import _getTextureSize::World -> Int -> Array Int

foreign import addTransformByProp::World -> Int -> FO.Object PropValue -> Effect Boolean

foreign import addRect2DByProp::World -> Int -> FO.Object PropValue -> Effect Boolean

foreign import addImageRenderByProp::World -> Int -> Int -> FO.Object PropValue -> Effect Boolean

foreign import getEvent::World -> Int -> Int -> Boolean -> Effect RawEvent

foreign import chainEventEffect::forall a. RawEvent -> (a -> Effect Unit) -> Effect Unit

foreign import setParent::World -> Int -> Int -> Effect Unit

foreign import chainEvent::forall a b. RawEvent -> (a -> b) -> RawEvent

foreign import _getViewPortSize::World -> Effect (Array Number)

foreign import _newBehavior::forall a. a -> RawBehavior

foreign import _attachBehavior::RawEvent -> RawBehavior -> Effect Unit

foreign import _setBehaviorFoldFunc::forall a b. RawBehavior -> (a -> b -> a) -> Effect Unit

foreign import _getBehaviorValue::forall a. RawBehavior -> a

foreign import _setBehaviorCallback::forall a. RawBehavior -> (a -> Effect Unit) -> Effect Unit

foreign import _setRect2dBehavior::World -> Entity -> FO.Object PropValue -> Effect Unit

foreign import _setTransformBehavior::World -> Entity -> FO.Object PropValue -> Effect Unit

















