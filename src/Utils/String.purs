module Utils.String where

-- import Prelude
import Data.String (split)
import Data.String.Pattern (Pattern(..))


newLinePattern :: Pattern
newLinePattern = Pattern "\n"

lines :: String -> Array String
lines str =
    split newLinePattern str
