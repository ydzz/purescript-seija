module Seija.Asset where

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Prelude (class Show, bind, pure, show, ($))
import Seija.App (AppReader)
import Seija.Foreign as F

data Asset2DType = Textute | Json | SpriteSheet | Font

assetTypeToId::Asset2DType -> Int
assetTypeToId Textute = 1
assetTypeToId Json = 2
assetTypeToId SpriteSheet = 3
assetTypeToId Font = 4

instance showAsset2DType :: Show Asset2DType where
   show Textute = "Texture"
   show Json = "Json"
   show SpriteSheet = "SpriteSheet"
   show Font = "Font"      

newtype Asset2D = Asset2D {
   resId::Int,
   assetType::Asset2DType
}

instance showAsset2D :: Show Asset2D where
   show (Asset2D {resId,assetType}) = "Asset(" <> show assetType <> ":" <> show resId <> ")"

type AssetPath = Tuple Asset2DType String

texturePath::String -> AssetPath
texturePath = Tuple Textute

jsonPath::String -> AssetPath
jsonPath = Tuple Json

spriteSheetPath::String -> AssetPath
spriteSheetPath = Tuple SpriteSheet

fontPath::String -> AssetPath
fontPath = Tuple Font


loadAssetSync::AssetPath -> AppReader Asset2D
loadAssetSync (Tuple assetType path) = do
    let id = assetTypeToId assetType
    handle <- ask
    assetId <- liftEffect $ F.loadAssetSync handle.world handle.loader id path
    pure $ Asset2D {resId:assetId, assetType}