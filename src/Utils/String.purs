module Utils.String where

import Prelude
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))


lines :: String -> Array String
lines str =
    String.split (Pattern "\n") str


isEmpty :: String -> Boolean
isEmpty str =
    if String.length str == 0 then
        true
    else
        false


removeAll :: String -> String -> String
removeAll pattern =
    String.replaceAll (Pattern pattern) (Replacement "")
