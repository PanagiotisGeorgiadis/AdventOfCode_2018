module Challenges.Day2 where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Array as Array
import Utils.Helpers (getInputLines)
import Utils.String as String


countCharacterOccurences :: String -> String -> Int
countCharacterOccurences char str = go str 0
    where go str_ occur
            | String.isEmpty str_        = occur
            | String.take 1 str_ == char = go (String.drop 1 str_) (occur + 1)
            | otherwise                  = go (String.drop 1 str_) occur


countOccurences :: forall a. Eq a => a -> Array a -> Int
countOccurences needle haystack = go haystack 0
    where go haystack_ count
            | Array.isEmpty haystack_              = count
            | Array.take 1 haystack_ == [ needle ] = go (Array.drop 1 haystack_) (count + 1)
            | otherwise                            = go (Array.drop 1 haystack_) count


getRowChecksum :: String -> Array Int
getRowChecksum line = go line []
    where go line_ occurs
            | String.isEmpty line_ = occurs
            | otherwise =
                let
                    targetChar =
                        String.take 1 line_

                    updatedOccurs =
                        Array.snoc occurs (countCharacterOccurences targetChar line_)
                in
                go (String.removeAll targetChar line_) updatedOccurs


calculateChecksum :: Array Int -> Int
calculateChecksum array = go (Array.sort array) []
    where go array_ occurs
            | Array.isEmpty array_ = Array.foldl (*) 1 occurs
            | otherwise            =
                let
                    pointer =
                        Array.head array_

                    updatedOccurs =
                        case pointer of
                            Just elem ->
                                Array.snoc occurs (countOccurences elem array_)

                            Nothing ->
                                occurs
                in
                case pointer of
                    Just elem ->
                        go (Array.filter ((/=) elem) array_) updatedOccurs

                    Nothing ->
                        go array_ updatedOccurs

-- 6448
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day2.txt")
    Console.log
        $ show
        $ calculateChecksum
        $ Array.concat
        $ map (Array.filter ((/=) 1) <<< Array.nub <<< getRowChecksum)
        $ getInputLines contents


type IdsComparison =
    { lhs :: String
    , rhs :: String
    , differences :: Int
    }

-- countSameChars :: String -> String -> Int
-- countSameChars lhs rhs = go lhs rhs 0
--     where go lhs_ rhs_ count
--             | String.isEmpty lhs_                      = count
--             | String.take 1 lhs_ == String.take 1 rhs_ = go (String.drop 1 lhs_) (String.drop 1 rhs_) (count + 1)
--             | otherwise                                = go (String.drop 1 lhs_) (String.drop 1 rhs_) count

keepCommonChars :: String -> String -> String
keepCommonChars lhs rhs = go lhs rhs ""
    where go lhs_ rhs_ res
            | String.isEmpty lhs_ = res
            | otherwise =
                let
                    lhsHead =
                        String.take 1 lhs_

                    rhsHead =
                        String.take 1 rhs_

                    dropChar =
                        String.drop 1
                in
                if lhsHead == rhsHead then
                    go (dropChar lhs_) (dropChar rhs_) (String.joinWith "" [ res, lhsHead ])
                else
                    go (dropChar lhs_) (dropChar rhs_) res


countDifferentChars :: String -> String -> Int
countDifferentChars lhs rhs = go lhs rhs 0
    where go lhs_ rhs_ count
            | String.isEmpty lhs_                      = count
            | String.take 1 lhs_ == String.take 1 rhs_ = go (String.drop 1 lhs_) (String.drop 1 rhs_) count
            | otherwise                                = go (String.drop 1 lhs_) (String.drop 1 rhs_) (count + 1)


getDifferencesForLine :: Array String -> String -> Array IdsComparison
getDifferencesForLine array str =
    Array.filter (\item -> item.differences /= 0) $
        map
            (\item ->
                { lhs : str
                , rhs : item
                , differences : countDifferentChars str item
                }
            )
            array


getDifferencesForInput :: Array String -> String
getDifferencesForInput array =
    let
        calculatedDifferences =
            map (getDifferencesForLine array) array
    in
    go 1 (Array.concat calculatedDifferences)
        where go diffs array_ =
                let
                    res =
                        Array.filter (\item -> item.differences == diffs) array_
                in
                if Array.isEmpty res then
                    go (diffs + 1) array_
                else
                    case Array.head res of
                        Just r ->
                            keepCommonChars r.lhs r.rhs

                        Nothing ->
                            ""

-- "evsialkqyiurohzpwucngttmf"
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day2.txt")
    Console.log
        $ show
        $ getDifferencesForInput
        $ getInputLines contents
