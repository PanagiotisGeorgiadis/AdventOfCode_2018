module Utils.Helpers where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Effect.Exception (Error)
import Utils.String as String


getInputLines :: Either Error String -> Array String
getInputLines result =
    case result of
        Right res ->
            Array.filter ((/=) "") $ String.lines res

        Left error ->
            []
