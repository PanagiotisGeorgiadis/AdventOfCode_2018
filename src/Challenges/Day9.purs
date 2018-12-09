module Challenges.Day9 where


import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (abs)
import Data.Tuple (Tuple(..))

type Marbles = Array Int
type Players = Map Int Int

incrementPlayerIndex :: Int -> Players -> Int
incrementPlayerIndex index players =
    if index == Map.size players then
        1
    else
        index + 1


getWinnerScore :: Players -> Int
getWinnerScore players =
    Array.foldl
        (\res value ->
            max res value
        )
        0
        (Array.fromFoldable $ Map.values players)


solve_ :: Players -> Marbles -> Int
solve_ players = go [0] 1 0 1 players
    where go board marbleIndex previousIndex playerIndex players_ remainingMarbles =
            let
                targetMarble =
                    Maybe.fromMaybe 0
                        $ Array.head remainingMarbles

                remainingMarbles_ =
                    Array.drop 1 remainingMarbles

                playerIndex_ =
                    incrementPlayerIndex playerIndex players
            in
            if targetMarble > 0 && mod targetMarble 23 == 0 then
                let
                    magicNumber =
                        7

                    removeIndex =
                        if previousIndex - magicNumber < 0 then
                            Array.length board - (abs (previousIndex - magicNumber))
                        else
                            previousIndex - magicNumber

                    extraMarble =
                        Maybe.fromMaybe 0
                            $ Array.head
                            $ Array.drop removeIndex board

                    updatedPlayers =
                        Map.update
                            (\v ->
                                Just (v + targetMarble + extraMarble)
                            )
                            playerIndex
                            players_

                    board_ =
                        Array.take removeIndex board
                            <> Array.drop (removeIndex + 1) board

                    marbleIndex_ =
                        if removeIndex + 2 > Array.length board_ then
                            1
                        else
                            removeIndex + 2
                in
                if Array.null remainingMarbles then
                    getWinnerScore players_
                else
                    go board_ marbleIndex_ removeIndex playerIndex_ updatedPlayers remainingMarbles_
            else
                let
                    marbleIndex_ =
                        if marbleIndex < Array.length board then
                            marbleIndex + 2
                        else
                            1

                    board_ =
                        Array.take marbleIndex board
                            <> Array.singleton targetMarble
                            <> Array.drop marbleIndex board
                in
                if Array.null remainingMarbles then
                    getWinnerScore players_
                else
                    go board_ marbleIndex_ marbleIndex playerIndex_ players_ remainingMarbles_

type Result =
    { board :: Marbles
    , marbleIndex :: Int
    , previousIndex :: Int
    , playerIndex :: Int
    , players :: Players
    }

solve :: Players -> Marbles -> Int
solve ps marbles =
    let
        thing =
            Array.foldl
                (\{ board, marbleIndex, previousIndex, playerIndex, players } targetMarble ->
                    let
                        playerIndex_ =
                            incrementPlayerIndex playerIndex players
                    in
                    if targetMarble > 0 && mod targetMarble 23 == 0 then
                        let
                            magicNumber =
                                7

                            removeIndex =
                                if previousIndex - magicNumber < 0 then
                                    Array.length board - (abs (previousIndex - magicNumber))
                                else
                                    previousIndex - magicNumber

                            extraMarble =
                                Maybe.fromMaybe 0
                                    $ Array.head
                                    $ Array.drop removeIndex board

                            updatedPlayers =
                                Map.update
                                    (\v ->
                                        Just (v + targetMarble + extraMarble)
                                    )
                                    playerIndex
                                    players

                            board_ =
                                Array.take removeIndex board
                                    <> Array.drop (removeIndex + 1) board

                            marbleIndex_ =
                                if removeIndex + 2 > Array.length board_ then
                                    1
                                else
                                    removeIndex + 2
                        in
                        { board : board_
                        , marbleIndex : marbleIndex_
                        , previousIndex : removeIndex
                        , playerIndex : playerIndex_
                        , players : updatedPlayers
                        }
                    else
                        let
                            marbleIndex_ =
                                if marbleIndex < Array.length board then
                                    marbleIndex + 2
                                else
                                    1

                            board_ =
                                Array.take marbleIndex board
                                    <> Array.singleton targetMarble
                                    <> Array.drop marbleIndex board
                        in
                        { board : board_
                        , marbleIndex : marbleIndex_
                        , previousIndex : marbleIndex
                        , playerIndex : playerIndex_
                        , players : players
                        }
                )
                ({ board : [0]
                , marbleIndex : 1
                , previousIndex : 0
                , playerIndex : 1
                , players : ps
                })
                marbles
    in
    getWinnerScore thing.players


-- 421237 ??? -- Too low
-- 420721
-- 434729 ??? -- Too high
-- 425688
firstChallenge :: Int
firstChallenge =
    let
        players =
            Map.fromFoldable
                $ map (\index -> Tuple index 0)
                $ Array.range 1 411

        marbles =
            Array.range 1 71170
    in
    solve_ players marbles

-- 1561260
secondChallenge :: Int
secondChallenge =
    let
        players =
            Map.fromFoldable
                $ map (\index -> Tuple index 0)
                $ Array.range 1 411

        marbles =
            Array.range 1 (71170 * 100)
    in
    solve_ players marbles
