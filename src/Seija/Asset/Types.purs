module Seija.Asset.Types where

import Seija.Foreign (class ToFFIJsObject)

class (ToFFIJsObject a,HasAssetTypeId a) <= LoaderInfo a b | a -> b where
  path::a -> String

class HasAssetTypeId a where
  assetTypeId::a -> Int


class Asset a where
  fromId::Int -> a

newtype Texture = Texture Int

instance assetTexture :: Asset Texture where
  fromId = Texture

newtype SpriteSheet = SpriteSheet Int

instance assetSpriteSheet :: Asset SpriteSheet where
  fromId = SpriteSheet

newtype Font = Font Int

instance assetFont :: Asset Font where
  fromId = Font