module Challenges.Day1 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try, Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Array as Array
import Utils.Maybe as Maybe
import Utils.String as String

getInputLines :: Either Error String -> Array String
getInputLines result =
    case result of
        Right res ->
            String.lines res

        Left error ->
            []

-- 543
parseFrequencies :: Array String -> Array Int
parseFrequencies =
    map
        (\line ->
            Maybe.withDefault 0 (fromString line)
        )


firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day1.txt")
    Console.log $ show $ Array.sum $ parseFrequencies $ getInputLines contents


calculateResults :: Array Int -> Int
calculateResults freqs = go freqs [0]
    where go rem past =
            let
                remItem =
                    Array.head rem

                latestResult =
                    Array.head $ Array.reverse past
            in
            case remItem, latestResult of
                Just item, Just latest ->
                    let
                        newResult =
                            latest + item
                    in
                    if (Array.any ((==) newResult) past) then
                        newResult
                    else
                        go (Array.drop 1 rem) (Array.snoc past (latest + item))

                _, _ ->
                    go freqs past


sanitizeFrequencies :: Array Int -> Array Int
sanitizeFrequencies =
    Array.filter ((/=) 0)

-- 621
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day1.txt")
    Console.log $ show $ calculateResults $ sanitizeFrequencies $ parseFrequencies $ getInputLines contents
