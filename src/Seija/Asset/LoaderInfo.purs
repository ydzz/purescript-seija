module Seija.Asset.LoaderInfo where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Foreign (unsafeToForeign)
import Foreign.Object as FO
import Seija.Asset.Texture (TextureConfig, mayTextureConfigToJsObject)
import Seija.Asset.Types (class HasAssetTypeId, class LoaderInfo, Font, SpriteSheet, Texture)
import Seija.Foreign (class ToFFIJsObject)

newtype TextureLoaderInfo = TextureLoaderInfo {
   path::String,
   config::Maybe TextureConfig
}

textureLoaderInfo::String -> Maybe TextureConfig -> TextureLoaderInfo
textureLoaderInfo path config = TextureLoaderInfo {path, config }

instance toFFIJsObjectTextureLoaderInfo :: ToFFIJsObject TextureLoaderInfo where
  toJsObject (TextureLoaderInfo info) = unsafeToForeign $ FO.fromFoldable ["path" /\ unsafeToForeign info.path,
                                                                           "config" /\ mayTextureConfigToJsObject info.config]
  

instance hasAssetTypeTextureLoadInfo :: HasAssetTypeId TextureLoaderInfo where
   assetTypeId _ = 1
   
instance loaderInfoTexture :: LoaderInfo TextureLoaderInfo Texture where
    path (TextureLoaderInfo info) = info.path


newtype SpriteSheetLoaderInfo = SpriteSheetLoaderInfo {
   path::String,
   config::Maybe TextureConfig
}

instance toFFIJsObjectSpriteSheetLoaderInfo :: ToFFIJsObject SpriteSheetLoaderInfo where
  toJsObject (SpriteSheetLoaderInfo info) = unsafeToForeign $ FO.fromFoldable ["path" /\ unsafeToForeign info.path,
                                                                                "config" /\ mayTextureConfigToJsObject info.config]

instance hasAssetTypeSpriteSheetLoadInfo :: HasAssetTypeId SpriteSheetLoaderInfo where
   assetTypeId _ = 2

instance loaderInfSpriteSheet :: LoaderInfo SpriteSheetLoaderInfo SpriteSheet where
    path (SpriteSheetLoaderInfo info) = info.path

spriteSheetLoaderInfo::String -> Maybe TextureConfig -> SpriteSheetLoaderInfo
spriteSheetLoaderInfo path config = SpriteSheetLoaderInfo {path, config }

newtype FontLoaderInfo = FontLoaderInfo {path::String }

instance toFFIJsObjectFontLoaderInfo :: ToFFIJsObject FontLoaderInfo where
  toJsObject (FontLoaderInfo info) = unsafeToForeign $ FO.fromFoldable ["path" /\ unsafeToForeign info.path,
                                                                        "config" /\ unsafeToForeign 0]

fontLoaderInfo::String -> FontLoaderInfo
fontLoaderInfo path = FontLoaderInfo {path }


instance loaderInfoFont :: LoaderInfo FontLoaderInfo Font where
    path (FontLoaderInfo info) = info.path

instance hasAssetTypeFontLoadInfo :: HasAssetTypeId FontLoaderInfo where
   assetTypeId _ = 3
