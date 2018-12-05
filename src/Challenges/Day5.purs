module Challenges.Day5 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.String as StringUtils


eliminateReactions :: String -> String
eliminateReactions = go false ""
    where go shouldRecurse res input =
            let
                first =
                    String.take 1 input

                second =
                    String.take 1 $ String.drop 1 input

                elementsReacted =
                    if String.null first || String.null second then
                        false
                    else
                        (String.toLower first == String.toLower second)
                            && (first /= second )

                updatedShouldRecurse =
                    if shouldRecurse || elementsReacted then
                        true
                    else
                        false

                updatedResult =
                    if elementsReacted then
                        res
                    else
                        res <> String.take 1 input

                updatedInput =
                    if elementsReacted then
                        String.drop 2 input
                    else
                        String.drop 1 input
            in
            if String.null input then
                if shouldRecurse then
                    go false "" res
                else
                    res
            else
                go updatedShouldRecurse updatedResult updatedInput

-- 21528 Too high
-- 9060 Correct.
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day5.txt")
    Console.log
        $ show
        $ String.length
        $ eliminateReactions
        $ Maybe.fromMaybe ""
        $ Array.head
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents



eliminateSpecificReaction :: String -> String -> String
eliminateSpecificReaction letter input = go input
    where go input_ =
            let
                reactedInput =
                    StringUtils.removeAll (String.toUpper letter)
                        $ StringUtils.removeAll (String.toLower letter) input
            in
            if String.length input_ == String.length reactedInput then
                input_
            else
                go reactedInput


findShortestPolymer :: String -> String
findShortestPolymer input =
    let
        inputLength =
            String.length input

        polymers =
            map eliminateReactions
            $ Array.filter (\item -> String.length item /= String.length input)
            $ map
                (\letter ->
                    eliminateSpecificReaction letter input
                )
                (StringUtils.alphabet)
    in
    Array.foldl
        (\res polymer ->
            if String.length polymer < String.length res then
                polymer
            else
                res
        )
        (String.joinWith "" $ Array.replicate inputLength "0")
        polymers


-- 6310
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day5.txt")
    Console.log
        $ show
        $ String.length
        $ findShortestPolymer
        $ Maybe.fromMaybe ""
        $ Array.head
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
