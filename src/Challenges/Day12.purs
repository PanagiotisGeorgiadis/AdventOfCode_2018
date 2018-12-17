module Challenges.Day12 where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as StringCodeUnits
import Data.String.CodeUnits as StringCodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.String as StringUtils
-- LLCRR => N
--

type GenerationPattern =
    { pattern :: String
    , result :: String
    }

initialState :: String
initialState =
    "#..#.#..##......###...###"


type State = Array StateItem

newtype StateItem = StateItem { index :: Int, value :: Char }

instance showStateItem :: Show StateItem
    where show (StateItem { value }) = show value

instance ordStateItem :: Ord StateItem
    where compare (StateItem lhs) (StateItem rhs) = compare lhs.index rhs.index

instance eqStateItem :: Eq StateItem
    where eq (StateItem lhs) (StateItem rhs) = lhs.value == rhs.value

initialState_ :: State
initialState_ =
    mapWithIndex
        (\index c ->
            StateItem { index : index, value : c }
        )
        (StringCodeUnits.toCharArray initialState)


parseGenerationPattern :: String -> GenerationPattern
parseGenerationPattern line =
    let
        data_ =
            map String.trim
                $ String.split (String.Pattern "=>") line
    in
    { pattern : Maybe.fromMaybe "" $ Array.head data_
    , result  : Maybe.fromMaybe "." $ Array.head $ Array.drop 1 data_
    }


type Tracker =
    { startIndex :: Int
    , score :: Int
    , state :: String
    }


-- getMatchingPattern :: Array GenerationPattern -> String -> Maybe GenerationPattern
-- getMatchingPattern patterns slice =
--     Array.head
--         $ Array.filter ((==) slice <<< _.pattern ) patterns

newStartIndex :: Int -> String -> Int
newStartIndex start state =
    -- let
    --     _ = Debug.trace (String.indexOf (String.Pattern "#") state) (\_ -> "")
    -- in
    case String.indexOf (String.Pattern "#") state of
        Just int -> start + int - 5
        Nothing  -> start


validPatterns :: String -> Array GenerationPattern -> Array GenerationPattern
validPatterns state =
    Array.filter
        (\p -> String.contains (String.Pattern p.pattern) state
        )


-- isEmpty :: String.CodePoint -> Boolean
-- isEmpty c =
--     case (Array.head $ String.toCodePointArray ".") of
--         Just c_ -> c == c_
--         Nothing -> false

getNextGeneration :: String -> Array GenerationPattern -> String
getNextGeneration state patterns =
    let
        stateIndexes = Array.range 0 (String.length state)
        nextGeneration =
            Array.foldl
                (\res index ->
                    let
                        slice = String.take 5 $ String.drop index state
                    in
                    if Array.null $ validPatterns slice patterns then
                        res <> "."
                    else
                        res <> "#"
                )
                ""
                stateIndexes

        _ = Debug.trace state (\_ -> "")
    in
    -- (String.fromCodePointArray
    --      $ Array.reverse
    --      $ String.toCodePointArray
    --      $ String.dropWhile (isEmpty)
    --      $ String.fromCodePointArray
    --      $ Array.reverse
    --      $ String.toCodePointArray
    --      $ String.dropWhile (isEmpty) nextGeneration
    --  )
    nextGeneration


sumState :: Int -> String -> Int
sumState start state =
    sum $
        (\(Tuple n c) ->
            if c == '#' then
                n
            else
                0
         )
         <$> Array.zip (Array.range start 1000000) (StringCodeUnits.toCharArray state)

type Temp =
    { state :: String
    , score :: Int
    , startIndex :: Int
    }

solve :: Array GenerationPattern -> String
solve patterns =
    _.state
        $ Array.foldl
            (\res _ ->
                let
                    -- startIndex_ = newStartIndex res.startIndex res.state
                    state_      = getNextGeneration res.state patterns
                    -- startIndex_ = res.startIndex + 5
                    -- score_      = sumState startIndex_ state_
                    score_      = sumState res.startIndex state_
                    --
                    _ = Debug.trace ("StartIndex: " <> show res.startIndex) (\_ -> "")
                    -- _ = Debug.trace ("Score: " <> show score_) (\_ -> "")
                in
                case String.indexOf (String.Pattern "#") state_ of
                    Just index ->
                        if index < 2 then
                            { state : "....." <> state_ <> "....."
                            , startIndex : res.startIndex + 5
                            , score : score_
                            }
                        else
                            { state : state_
                            , startIndex : res.startIndex
                            , score : score_
                            }
                    Nothing ->
                        { state : state_
                        , startIndex : res.startIndex
                        , score : score_
                        }
            )
            ( { state : "....." <> initialState <> "....."
              , startIndex : 0
              , score : 0
              }
            )
            (Array.range 1 20)


getMinIndex :: State -> Int
getMinIndex =
    Array.foldl
        (\res (StateItem item) ->
            min res item.index
        )
        1

getMaxIndex :: State -> Int
getMaxIndex =
    Array.foldl
        (\res (StateItem item) ->
            max res item.index
        )
        0

padLeft :: State -> State
padLeft state =
    let
        minIndex = getMinIndex state
        padding  =
            [ StateItem { index : minIndex - 1, value : '.' }
            , StateItem { index : minIndex - 2, value : '.' }
            , StateItem { index : minIndex - 3, value : '.' }
            , StateItem { index : minIndex - 4, value : '.' }
            , StateItem { index : minIndex - 5, value : '.' }
            ]
    in
    Array.sort (state <> padding)


padRight :: State -> State
padRight state =
    let
        maxIndex = getMaxIndex state
        padding  =
            [ StateItem { index : maxIndex + 1, value : '.' }
            , StateItem { index : maxIndex + 2, value : '.' }
            , StateItem { index : maxIndex + 3, value : '.' }
            , StateItem { index : maxIndex + 4, value : '.' }
            , StateItem { index : maxIndex + 5, value : '.' }
            ]
    in
    Array.sort (state <> padding)


getNextGeneration_ :: State -> Array GenerationPattern -> State
getNextGeneration_ state patterns =
    let
        stateIndexes      = Array.range 0 (Array.length state)
        nextGenerationStr =
            Array.foldl
                (\res index ->
                    let
                        slice =
                            StringCodeUnits.fromCharArray
                                $ map (\(StateItem item) -> item.value )
                                $ Array.take 5
                                $ Array.drop index state
                    in
                    if Array.null $ validPatterns slice patterns then
                        res <> "."
                    else
                        res <> "#"
                )
                ""
                stateIndexes

        -- _ = Debug.trace nextGeneration (\_ -> "")
        nextGen =
            map
                (\(Tuple (StateItem item) newVal) ->
                    StateItem { index : item.index, value : newVal }
                )
                (Array.zip state (StringCodeUnits.toCharArray nextGenerationStr))
    in
    -- (String.fromCodePointArray
    --      $ Array.reverse
    --      $ String.toCodePointArray
    --      $ String.dropWhile (isEmpty)
    --      $ String.fromCodePointArray
    --      $ Array.reverse
    --      $ String.toCodePointArray
    --      $ String.dropWhile (isEmpty) nextGeneration
    --  )
    nextGen

solve_ :: Array GenerationPattern -> String
solve_ patterns =
    let
        -- _ = Debug.trace "" (\_ -> "")
        state   = padRight $ padLeft initialState_
        -- nextGen = getNextGeneration_ state patterns

        -- _ = Debug.trace state (\_ -> "")
        -- _ = Debug.trace nextGen (\_ -> "")
    in
    StringCodeUnits.fromCharArray
        $ map (\(StateItem item) -> item.value)
        $ Array.foldl
            (\res _ ->
                let
                    -- str = StringCodeUnits.fromCharArray $ map (\(StateItem item) -> item.value) res
                    _ = Debug.trace res (\_ -> "")
                in
                getNextGeneration_ (padRight $ padLeft res) patterns
            )
            state
            (Array.range 1 20)


-- let
--     -- startIndex_ = newStartIndex res.startIndex res.state
--     state_      = getNextGeneration res.state patterns
--     -- startIndex_ = res.startIndex + 5
--     -- score_      = sumState startIndex_ state_
--     score_      = sumState res.startIndex state_
--     --
--     _ = Debug.trace ("StartIndex: " <> show res.startIndex) (\_ -> "")
--     -- _ = Debug.trace ("Score: " <> show score_) (\_ -> "")
-- in
-- case String.indexOf (String.Pattern "#") state_ of
--     Just index ->
--         if index < 2 then
--             { state : "....." <> state_ <> "....."
--             , startIndex : res.startIndex + 5
--             , score : score_
--             }
--         else
--             { state : state_
--             , startIndex : res.startIndex
--             , score : score_
--             }
--     Nothing ->
--         { state : state_
--         , startIndex : res.startIndex
--         , score : score_
--         }
-- )

firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day12_patterns_e.txt")
    Console.log
        -- $ show
        -- $ getNextGeneration initialState
        $ solve_
        $ map parseGenerationPattern
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents



secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day12_patterns_e.txt")
    Console.log
        $ show
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
