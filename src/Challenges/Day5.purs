module Challenges.Day5 where

import Prelude

import Control.Monad.Gen (resize)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.Maybe as Maybe
import Utils.String as StringUtils


type thingTracker =
    { shouldRecurse :: Bool
    , polimer :: Array CodePoint
    , previous :: CodePoint
    }

calculateReactions :: String -> Array CodePoint
calculateReactions = go False (String.toCodePointArray input)
    go shouldRecurse input =
        let
            chars =
                String.take 2 input

            updatedShouldRecurse =
                if String.toLower (String.singleton item) == String.toLower (String.singleton res.previous) then
                    

            thing =
                Array.foldl
                    (\res item ->
                        if String.toLower (String.singleton item) == String.toLower (String.singleton res.previous) then

                    )
                    { shouldRecurse : False
                    ,
                    }
                    input
        in
        if thing.shouldRecurse then
            go thing.input
        else
            String.fromCodePointArray thing.input


firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day5.txt")
    Console.log
        $ show
        $ calculateReactions
        $ Maybe.withDefault ""
        $ Array.head
        -- $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day5.txt")
    Console.log
        $ show
        -- $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
