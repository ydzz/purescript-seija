module Seija.Foreign  (
    appVersion
) where

foreign import appVersion::String

foreign import newSimple2d::String -> String
