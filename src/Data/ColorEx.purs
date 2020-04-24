module Data.ColorEx where

import Color (Color, toRGBA')

toNumberArray::Color -> Array Number
toNumberArray color = [rgba.r,rgba.g,rgba.b,rgba.a]
    where
     rgba = toRGBA' color