module Main where

import Prelude

-- import Effect.Exception (try)
-- import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)

import Challenges.Day1 (firstChallenge)


main :: Effect Unit
main = do
    day1a <- firstChallenge
    Console.log "YOYO"
