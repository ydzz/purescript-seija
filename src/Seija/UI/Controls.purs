module Seija.UI.Controls where

import Prelude
import Data.Typelevel.Undefined (undefined)
import Seija.App (AppReader)
import Seija.FRP (Event)
import Seija.Foreign (Entity)

button::String -> AppReader (Event Entity)
button txt = do
    pure $ undefined