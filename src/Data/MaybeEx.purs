module Data.MaybeEx where
import Data.Maybe (Maybe(..))

maybeToList :: forall a. Maybe a -> Array a
maybeToList Nothing  = []
maybeToList (Just x) = [x]