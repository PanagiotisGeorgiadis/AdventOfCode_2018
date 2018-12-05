module Challenges.Day1 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try, Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.String as StringUtils


parseFrequencies :: Array String -> Array Int
parseFrequencies =
    map (Maybe.fromMaybe 0 <<< Int.fromString)


-- 543
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day1.txt")
    Console.log
        $ show
        $ sum
        $ parseFrequencies
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


type DuplicationDetector =
        { frequencyHistory :: Set Int
        , latestFreq :: Int
        , shouldTerminate :: Boolean
        }


initialDuplicateDetector :: DuplicationDetector
initialDuplicateDetector =
        { frequencyHistory : (Set.fromFoldable [0])
        , latestFreq : 0
        , shouldTerminate : false
        }


findDuplicateFrequency :: Array Int -> Int
findDuplicateFrequency = go initialDuplicateDetector
    where go duplicateDetector freqs =
            let
                detector =
                    Array.foldl
                        (\detector item ->
                            let
                                newFrequency =
                                    detector.latestFreq + item
                            in
                            if detector.shouldTerminate then
                                detector

                            else if Set.member newFrequency detector.frequencyHistory then
                                detector
                                    { shouldTerminate = true
                                    , latestFreq = newFrequency
                                    , frequencyHistory = Set.insert (newFrequency + 1) detector.frequencyHistory
                                    }

                            else
                                detector
                                    { shouldTerminate = false
                                    , latestFreq = newFrequency
                                    , frequencyHistory = Set.insert newFrequency detector.frequencyHistory
                                    }
                        )
                        duplicateDetector
                        freqs
            in
            if detector.shouldTerminate then
                detector.latestFreq
            else
                go detector freqs


-- 621
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day1.txt")
    Console.log
        $ show
        $ findDuplicateFrequency
        $ parseFrequencies
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
