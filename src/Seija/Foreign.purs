module Seija.Foreign  where


import Color (Color)
import Color.Scheme.X11 (darkgray)
import Data.ColorEx (toNumberArray)
import Data.Default (class Default, default)
import Data.Maybe (Maybe(..), fromJust)
import Data.MaybeEx (maybeToList)
import Data.Monoid ((<>))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, map, ($), (<<<))

class ToJsObject a where
    toJsObject::a -> FO.Object Foreign
  
type WindowConfigRecord = {
    width::Int,
    height::Int,
    bgColor:: Maybe Color
}

newtype WindowConfig = WindowConfig WindowConfigRecord
derive instance newtypeWindowConfig :: Newtype WindowConfig _

mapWindow::WindowConfig -> (WindowConfigRecord -> WindowConfigRecord) -> WindowConfig
mapWindow (WindowConfig cfg) f = WindowConfig $ f cfg


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


newtype AppConfig = AppConfig {
    onStart::Maybe  (Effect Unit),
    onUpdate::Maybe (Effect Unit),
    onQuit::Maybe (Effect Unit),
    resPath::Maybe String
}

instance defaultAppConfig :: Default AppConfig where
    default::AppConfig
    default = AppConfig {
        onStart:Nothing,
        onUpdate:Nothing,
        onQuit:Nothing,
        resPath:Nothing
    }

instance toJsObjectAppConfig :: ToJsObject AppConfig where
    toJsObject (AppConfig cfg) = FO.fromFoldable ["OnStart" /\  (toForeign $ unsafePartial $ fromJust cfg.onStart),
                                                  "OnUpdate" /\ (toForeign $ unsafePartial $ fromJust cfg.onUpdate)
                                                 ]


foreign import appVersion::String
foreign import data Simple2d:: Type
foreign import data App:: Type

foreign import _newSimple2d::FO.Object Foreign -> Simple2d

newSimple2d::Simple2dConfig -> Simple2d
newSimple2d cfg = _newSimple2d (toJsObject cfg)

newApp::Simple2d -> AppConfig -> App
newApp s2d cfg = _newApp s2d (toJsObject cfg)

foreign import _newApp::Simple2d -> FO.Object Foreign -> App

foreign import runApp::App -> Effect Unit