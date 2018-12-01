module Utils.Maybe where

import Data.Maybe (Maybe(..))

withDefault :: forall a. a -> Maybe a -> a
withDefault default (Just val) = val
withDefault default Nothing    = default
