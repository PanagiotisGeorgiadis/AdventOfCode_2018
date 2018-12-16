{-# LANGUAGE FlexibleContexts #-}

module Day14 where

import Prelude


import qualified Data.Sequence as Seq
import qualified Data.Maybe    as Maybe

import qualified Data.Int      as Int
import qualified Data.List     as List
import qualified Data.Text     as Text

import Data.Foldable (foldl', toList)
import Data.Sequence (Seq)
import Data.Maybe    (Maybe(..))
import Text.Read     (readMaybe)
import Data.Char     (digitToInt)


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

data ElfData = ElfData
    { currentIndex :: Int
    , recipeScore :: Int
    } deriving Show

data ElfType = First Int | Second Int
    deriving Show

data Occupant = Occupied ElfType | Empty Int
    deriving Show

data ScoreBoardItem = ScoreBoardItem
    { index :: Int
    , occupant :: Occupant
    } deriving Show

type ScoreBoard = Seq ScoreBoardItem


puzzleInput :: String
puzzleInput = "077201"
-- puzzleInput = "59414"

{-
getRecipeScore :: Maybe ScoreBoardItem -> Int
getRecipeScore Nothing                      = 0
getRecipeScore (Just (ScoreBoardItem item)) =
    case item.occupant of
        Occupied (First recipeScore)  -> recipeScore
        Occupied (Second recipeScore) -> recipeScore
        Empty recipeScore             -> recipeScore
-}
getRecipeScore :: ScoreBoardItem -> Int
getRecipeScore (ScoreBoardItem index occupant) =
    case occupant of
        Occupied (First recipeScore)  -> recipeScore
        Occupied (Second recipeScore) -> recipeScore
        Empty recipeScore             -> recipeScore


initialScoreBoard :: ScoreBoard
initialScoreBoard =
    Seq.fromList
        [ ScoreBoardItem { index = 0, occupant = Occupied (First 3) }
        , ScoreBoardItem { index = 1, occupant = Occupied (Second 7) }
        ]


createNewRecipes :: ScoreBoard -> ScoreBoard
createNewRecipes scoreBoard =
    let
        elfScores =
            sum
            $ foldl'
                (\res (ScoreBoardItem index occupant) ->
                    case occupant of
                        Occupied (First recipeScore) ->
                            res ++ [ recipeScore ]

                        Occupied (Second recipeScore) ->
                            res ++ [ recipeScore ]

                        Empty _ ->
                            res
                )
                []
                scoreBoard

        scores_ =
            if elfScores > 9 then
                map (digitToInt) (show elfScores)
            else
                [ elfScores ]

        newItems =
            Seq.mapWithIndex
                (\index score ->
                    ScoreBoardItem
                        { index = Seq.length scoreBoard + index
                        , occupant = Empty score
                        }
                )
                (Seq.fromList scores_)
    in
    scoreBoard <> newItems


getFirstElfData :: ScoreBoard -> Maybe ElfData
getFirstElfData scoreBoard =
    let
        elfData =
            Maybe.mapMaybe
                (\(ScoreBoardItem index occupant) ->
                    case occupant of
                        Occupied (First recipeScore) ->
                            Just (ElfData index recipeScore)

                        _ ->
                            Nothing
                )
                $ toList scoreBoard
    in
    if null elfData then
        Nothing
    else
        Just (head elfData)

getSecondElfData :: ScoreBoard -> Maybe ElfData
getSecondElfData scoreBoard =
    let
        elfData =
            Maybe.mapMaybe
                (\(ScoreBoardItem index occupant) ->
                    case occupant of
                        Occupied (Second recipeScore) ->
                            Just (ElfData index recipeScore)

                        _ ->
                            Nothing
                )
                $ toList scoreBoard
    in
    if null elfData then
        Nothing
    else
        Just (head elfData)


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
        scoreBoardLength = Seq.length scoreBoard

        updatedScoreBoard =
            case (firstElf, secondElf) of
                (Just (ElfData index score), Just (ElfData index_ score_ )) ->
                    let
                        newFirstIndex =
                            getNewIndex index (magicNumber + score) scoreBoardLength

                        newSecondIndex =
                            getNewIndex index_ (magicNumber + score_) scoreBoardLength

                        newFirstScore =
                            getRecipeScore
                                $ Seq.index scoreBoard newFirstIndex

                        newSecondScore =
                            getRecipeScore
                                $ Seq.index scoreBoard newSecondIndex
                    in
                    Seq.update newSecondIndex
                        ( ScoreBoardItem
                            { index = newSecondIndex
                            , occupant = Occupied (Second newSecondScore)
                            }
                        )
                        $ Seq.update newFirstIndex
                            ( ScoreBoardItem
                                { index = newFirstIndex
                                , occupant = Occupied (First newFirstScore)
                                }
                            )
                        $ Seq.update index_
                            ( ScoreBoardItem
                                { index = index_
                                , occupant = Empty score_
                                }
                            )
                        $ Seq.update index
                            ( ScoreBoardItem
                                { index = index
                                , occupant = Empty score
                                }
                            )
                            scoreBoard

                _ ->
                    scoreBoard
    in
    updatedScoreBoard

printBoard :: ScoreBoard -> String
printBoard scoreBoard =
    List.intercalate ""
    --concat
        $ map
        (\(ScoreBoardItem item occupant) ->
            case occupant of
                Occupied (First recipeScore)  -> show recipeScore
                Occupied (Second recipeScore) -> show recipeScore
                Empty recipeScore             -> show recipeScore
        )
        $ toList scoreBoard


drawScoreBoardForN :: Int -> ScoreBoard -> String
drawScoreBoardForN n scoreBoard =
    if Seq.length scoreBoard > n then
        printBoard scoreBoard
    else
        drawScoreBoardForN n (moveElves $ createNewRecipes scoreBoard)


-- 9211134315
firstChallenge :: String
firstChallenge =
    let
        puzzleInput_ = 77201
        scoreBoard = initialScoreBoard
    in
    take 10
        $ drop puzzleInput_
        $ drawScoreBoardForN (puzzleInput_ + 10) scoreBoard



drawScoreBoardWhile :: ScoreBoard -> Int
drawScoreBoardWhile scoreBoard =
    let
        scoreBoardStr = printBoard scoreBoard
    in
    if List.isInfixOf puzzleInput scoreBoardStr then
        length scoreBoardStr - length puzzleInput
    else
        drawScoreBoardWhile (moveElves $ createNewRecipes scoreBoard)


secondChallenge :: String
secondChallenge =
    show $ drawScoreBoardWhile initialScoreBoard
    -- show $ drop 2018 $ drawScoreBoardForN (2018 + 5) initialScoreBoard
