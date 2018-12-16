module Challenges.Day14 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List.Lazy.Types (List)
import Data.List.Lazy as List
-- import Data.List as List
-- import Data.List (List)
import Data.Map (Map(..), values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (abs)
import Data.String as String
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


-- Recipe score quality 0-9
-- First score = 3
-- Second score = 7
-- Two parallel recipes
--


-- newtype Recipe = Recipe
--     { previousIndex :: Int
--     , nextIndex :: Int
--     , value :: String
--     }


type Elf = { recipeScore :: Int }
type ElfData = { currentIndex :: Int, recipeScore :: Int }

data ElfType = First Elf | Second Elf

instance showElfType :: Show ElfType
    where show (First elf)  = "( First " <> show elf <> ")"
          show (Second elf) = "( Second " <> show elf <> ")"


data Occupant = Occupied ElfType | Empty Int

instance showOccupant :: Show Occupant
    where show (Empty score)      = "( Empty " <> show score <> ")"
          show (Occupied elfType) = "( Occupied " <> show elfType <> ")"

newtype ScoreBoardItem = ScoreBoardItem { index :: Int, occupant :: Occupant }

instance showScoreBoardItem :: Show ScoreBoardItem
    where show (ScoreBoardItem item) = "( ScoreBoardItem " <> show item <> ")"

type ScoreBoard = List ScoreBoardItem


puzzleInput :: String
puzzleInput = "077201"


getRecipeScore :: Maybe ScoreBoardItem -> Int
getRecipeScore Nothing                      = 0
getRecipeScore (Just (ScoreBoardItem item)) =
    case item.occupant of
        Occupied (First { recipeScore })  -> recipeScore
        Occupied (Second { recipeScore }) -> recipeScore
        Empty recipeScore                 -> recipeScore


initialScoreBoard :: ScoreBoard
initialScoreBoard =
    List.fromFoldable
        [ ScoreBoardItem { index : 0, occupant : Occupied (First { recipeScore : 3 }) }
        , ScoreBoardItem { index : 1, occupant : Occupied (Second { recipeScore : 7 }) }
        ]
    -- [ ScoreBoardItem { index : 0, occupant : Occupied (First { recipeScore : 0 }) }
    -- , ScoreBoardItem { index : 1, occupant : Occupied (Second { recipeScore : 7 }) }
    -- , ScoreBoardItem { index : 2, occupant : Empty 7 }
    -- , ScoreBoardItem { index : 2, occupant : Empty 2 }
    -- , ScoreBoardItem { index : 2, occupant : Empty 0 }
    -- , ScoreBoardItem { index : 2, occupant : Empty 1 }
    -- ]
-- initialScoreBoard = [ 0, 7, 7, 2, 0, 1 ]


createNewRecipes :: ScoreBoard -> ScoreBoard
createNewRecipes scoreBoard =
    let
        elfScores =
            sum
            $ List.foldl
                (\res (ScoreBoardItem item) ->
                    case item.occupant of
                        Occupied (First { recipeScore }) ->
                            List.snoc res recipeScore

                        Occupied (Second { recipeScore }) ->
                            List.snoc res recipeScore

                        Empty _ ->
                            res
                )
                (List.fromFoldable [])
                scoreBoard

        scores_ =
            if elfScores > 9 then
                List.fromFoldable
                    $ map (Maybe.fromMaybe 0 <<< Int.fromString)
                    $ String.split (String.Pattern "")
                    $ show elfScores
            else
                List.singleton elfScores

        newItems =
            mapWithIndex
                (\index score ->
                    ScoreBoardItem
                        { index : List.length scoreBoard + index
                        , occupant : Empty score
                        }
                )
                scores_
    in
    scoreBoard <> newItems


getFirstElfData :: ScoreBoard -> Maybe ElfData
getFirstElfData =
    List.head <<<
        List.mapMaybe
            (\(ScoreBoardItem item) ->
                case item.occupant of
                    Occupied (First { recipeScore }) ->
                        Just { currentIndex : item.index, recipeScore : recipeScore }

                    _ ->
                        Nothing
            )

getSecondElfData :: ScoreBoard -> Maybe ElfData
getSecondElfData =
    List.head <<<
        List.mapMaybe
            (\(ScoreBoardItem item) ->
                case item.occupant of
                    Occupied (Second { recipeScore }) ->
                        Just { currentIndex : item.index, recipeScore : recipeScore }

                    _ ->
                        Nothing
            )


getNewIndex :: Int -> Int -> Int -> Int
getNewIndex index moveTimes scoreBoardLength =
    if moveTimes == 0 then
        index
    else
        let
            updatedMoveTimes = moveTimes - 1
            updatedIndex     = index + 1
            go               = \i -> getNewIndex i updatedMoveTimes scoreBoardLength
        in
        if updatedIndex > scoreBoardLength - 1 then
            go 0
        else
            go updatedIndex


moveElves :: ScoreBoard -> ScoreBoard
moveElves scoreBoard =
    let
        magicNumber = 1
        firstElf  = getFirstElfData scoreBoard
        secondElf = getSecondElfData scoreBoard
        scoreBoardLength = List.length scoreBoard

        updatedScoreBoard =
            case firstElf, secondElf of
                Just first, Just second ->
                    let
                        newFirstIndex =
                            getNewIndex first.currentIndex (magicNumber + first.recipeScore) scoreBoardLength

                        newSecondIndex =
                            getNewIndex second.currentIndex (magicNumber + second.recipeScore) scoreBoardLength

                        newFirstScore =
                            getRecipeScore $ List.index scoreBoard newFirstIndex

                        newSecondScore =
                            getRecipeScore $ List.index scoreBoard newSecondIndex
                    in
                     -- Maybe.fromMaybe scoreBoard
                        List.updateAt newSecondIndex (ScoreBoardItem { index : newSecondIndex, occupant : Occupied (Second { recipeScore : newSecondScore })})
                        -- $ Maybe.fromMaybe scoreBoard
                        $ List.updateAt newFirstIndex (ScoreBoardItem { index : newFirstIndex, occupant : Occupied (First { recipeScore : newFirstScore })})
                        -- $ Maybe.fromMaybe scoreBoard
                        $ List.updateAt second.currentIndex (ScoreBoardItem { index : second.currentIndex, occupant : Empty second.recipeScore })
                        -- $ Maybe.fromMaybe scoreBoard
                        $ List.updateAt first.currentIndex (ScoreBoardItem { index : first.currentIndex, occupant : Empty first.recipeScore }) scoreBoard

                _, _ ->
                    scoreBoard
    in
    updatedScoreBoard

printBoard :: ScoreBoard -> String
printBoard s =
    String.joinWith ""
        $ map (\(ScoreBoardItem item) ->
                case item.occupant of
                    Occupied (First { recipeScore })  -> show recipeScore
                    Occupied (Second { recipeScore }) -> show recipeScore
                    Empty score                       -> show score
            )
        $ Array.fromFoldable s


drawScoreBoardForN :: Int -> ScoreBoard -> String
drawScoreBoardForN n scoreBoard =
    if List.length scoreBoard > n then
        printBoard scoreBoard
    else
        drawScoreBoardForN n (moveElves $ createNewRecipes scoreBoard)


-- 71 47 14 81 88
-- 71 47 14 81 88
firstChallenge :: String
firstChallenge =
    let
        puzzleInput_ = 77201
        scoreBoard = initialScoreBoard
    in
    String.take 10
        $ String.drop puzzleInput_
        $ drawScoreBoardForN (puzzleInput_ + 10) scoreBoard


secondChallenge :: String
secondChallenge =
    ""
