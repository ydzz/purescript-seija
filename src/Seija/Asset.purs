module Seija.Asset where
import Data.Array as A
import Data.Int (toNumber)
import Data.Tuple.Nested (Tuple4, (/\))
import Data.Vec (vec2)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, pure, unit, ($))
import Seija.App (class MonadApp, askWorld)
import Seija.Asset.Types (class Asset, class LoaderInfo, SpriteSheet(..), Texture(..), assetTypeId, fromId, path)
import Seija.Foreign (World, fetchLoader, toJsObject)
import Seija.Foreign as F
import Seija.Math.Vector (Vector2f)

loadAsset::forall m l a. MonadApp m => LoaderInfo l a => Asset a => l -> m a
loadAsset loaderInfo = do
  let typId = assetTypeId loaderInfo
  world <- askWorld
  loader <- liftEffect $  fetchLoader world
  id <- liftEffect $ F.loadAssetSync world loader typId (path loaderInfo) (toJsObject loaderInfo)
  pure $ fromId id


getTextureSize::forall m. (MonadApp m) => Texture -> m Vector2f
getTextureSize (Texture id) = do
   world <- askWorld
   let arr = F._getTextureSize world id
   let w = toNumber $ unsafePartial $ A.unsafeIndex arr 0
   let h = toNumber $ unsafePartial $ A.unsafeIndex arr 1
   pure $ vec2 w h

getTextureSizeWorld::World -> Texture -> Vector2f
getTextureSizeWorld world (Texture id) = vec2 w h
   where
      arr = F._getTextureSize world id
      w = toNumber $ unsafePartial $ A.unsafeIndex arr 0
      h = toNumber $ unsafePartial $ A.unsafeIndex arr 1

getSpirteRectInfo::World -> SpriteSheet -> String -> Tuple4 Number Number Number Number
getSpirteRectInfo world (SpriteSheet id) spriteName = x /\ y /\ w /\ h /\ unit
   where 
    arr = F._getSpriteRectInfo world id spriteName
    x = unsafePartial $ A.unsafeIndex arr 0
    y = unsafePartial $ A.unsafeIndex arr 1
    w = unsafePartial $ A.unsafeIndex arr 2
    h = unsafePartial $ A.unsafeIndex arr 3


