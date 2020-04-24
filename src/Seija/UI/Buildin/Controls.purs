module Seija.UI.Buildin.Controls where

import Prelude
import Seija.App (AppReader)
import Seija.FRP (Behavior(..))
import Seija.Foreign (Entity)

checkBox::Behavior Boolean -> AppReader Entity
checkBox b = pure 0
