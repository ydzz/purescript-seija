module Seija.Asset where

import Data.Array as A
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, (/\))
import Data.Vec (vec2)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, bind, pure, show, unit, ($))
import Seija.App (class MonadApp, askWorld)
import Seija.Foreign (class ToJsObject, World, fetchLoader)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f)

data Asset2DType = Textute | Json | SpriteSheet | Font

assetTypeToId::Asset2DType -> Int
assetTypeToId Textute = 1
assetTypeToId Json = 0
assetTypeToId SpriteSheet = 2
assetTypeToId Font = 3

instance showAsset2DType :: Show Asset2DType where
   show Textute = "Texture"
   show Json = "Json"
   show SpriteSheet = "SpriteSheet"
   show Font = "Font"      

newtype Asset2D = Asset2D {
   assetId::Int,
   assetType::Asset2DType
}

instance showAsset2D :: Show Asset2D where
   show (Asset2D {assetId,assetType}) = "Asset(" <> show assetType <> ":" <> show assetId <> ")"

type AssetPath = Tuple Asset2DType String

asset2dId::Asset2D -> Int
asset2dId (Asset2D asset) = asset.assetId


texturePath::String -> AssetPath
texturePath = Tuple Textute

jsonPath::String -> AssetPath
jsonPath = Tuple Json

spriteSheetPath::String -> AssetPath
spriteSheetPath = Tuple SpriteSheet

fontPath::String -> AssetPath
fontPath = Tuple Font


loadAssetSync::forall m.(MonadApp m) => AssetPath -> m Asset2D
loadAssetSync (Tuple assetType path) = do
    let typId = assetTypeToId assetType
    world <- askWorld
    loader <- liftEffect $  fetchLoader world
    id <- liftEffect $ F.loadAssetSync world loader typId path
    pure $ Asset2D {assetId:id, assetType}


getTextureSize::forall m. (MonadApp m) => Asset2D -> m Vector2f
getTextureSize (Asset2D asset) = do
   world <- askWorld
   let arr = F._getTextureSize world asset.assetId
   let w = toNumber $ unsafePartial $ A.unsafeIndex arr 0
   let h = toNumber $ unsafePartial $ A.unsafeIndex arr 1
   pure $ vec2 w h

getTextureSizeWorld::World -> Asset2D -> Vector2f
getTextureSizeWorld world (Asset2D asset) = vec2 w h
   where
      arr = F._getTextureSize world asset.assetId
      w = toNumber $ unsafePartial $ A.unsafeIndex arr 0
      h = toNumber $ unsafePartial $ A.unsafeIndex arr 1

getSpirteRectInfo::World -> Asset2D -> String -> Tuple4 Number Number Number Number
getSpirteRectInfo world (Asset2D asset) spriteName = x /\ y /\ w /\ h /\ unit
   where 
    arr = F._getSpriteRectInfo world asset.assetId spriteName
    x = unsafePartial $ A.unsafeIndex arr 0
    y = unsafePartial $ A.unsafeIndex arr 1
    w = unsafePartial $ A.unsafeIndex arr 2
    h = unsafePartial $ A.unsafeIndex arr 3


class (ToJsObject a) <= LoaderInfo a

newtype TextureLoaderInfo = TextureLoaderInfo {
   path::String,
   config::Maybe TextureConfig
}

newtype SpriteSheetLoaderInfo = SpriteSheetLoaderInfo {
   path::String,
   config::Maybe TextureConfig
}

newtype TextureConfig = TextureConfig {

}