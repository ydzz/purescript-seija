module Seija.UI.Controls where

import Prelude

import Data.Typelevel.Undefined (undefined)
import Seija.App (class MonadApp)
import Seija.FRP (Event)
import Seija.Foreign (Entity)

button::forall m. (MonadApp m) => String -> m (Event Entity)
button txt = do
    pure $ undefined