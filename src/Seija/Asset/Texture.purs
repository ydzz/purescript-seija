module Seija.Asset.Texture where

import Prelude

import Data.Default (class Default, default)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as FO
import Seija.Foreign (class ToFFIJsObject, toJsObject)

data Filter = Linear | Nearest
data WrapMode = Tile | Mirror | Clamp | Border

newtype SamplerDesc = SamplerDesc {
    wrap_mode::WrapMode,
    filter::Filter
}

newtype TextureConfig = TextureConfig {
    sampler_info:: SamplerDesc,
    generate_mips:: Boolean,
    premultiply_alpha:: Boolean
}

instance defaultTextureConfig :: Default TextureConfig where
    default = TextureConfig {
        sampler_info:  default,
        generate_mips: false,
        premultiply_alpha: false
    }

instance defaultSamplerDesc :: Default SamplerDesc where
    default = SamplerDesc {wrap_mode:Clamp,filter:Linear }

instance toFFIJsObjectTextureConfig :: ToFFIJsObject TextureConfig where
    toJsObject (TextureConfig cfg) = unsafeToForeign $ FO.fromFoldable ["generate_mips" /\ unsafeToForeign cfg.generate_mips,
                                                                        "premultiply_alpha" /\ unsafeToForeign cfg.premultiply_alpha,
                                                                        "sampler_info" /\ toJsObject cfg.sampler_info]

instance toFFIJsObjectSamplerDesc :: ToFFIJsObject SamplerDesc where
    toJsObject (SamplerDesc desc) = unsafeToForeign $ [toJsObject desc.filter,toJsObject desc.wrap_mode]

instance toFFIJsObjectFilter :: ToFFIJsObject Filter where
    toJsObject Nearest = unsafeToForeign 0
    toJsObject Linear  = unsafeToForeign 1

instance toFFIJsObjectWrap :: ToFFIJsObject WrapMode where
    toJsObject Tile      = unsafeToForeign 0
    toJsObject Mirror    = unsafeToForeign 1
    toJsObject Clamp     = unsafeToForeign 2
    toJsObject Border    = unsafeToForeign 3
    
mayTextureConfigToJsObject::Maybe TextureConfig -> Foreign
mayTextureConfigToJsObject Nothing  = unsafeToForeign 0
mayTextureConfigToJsObject (Just cfg) = toJsObject cfg