module Challenges.Day9 where


import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (abs, max)
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

                    -- _ = Debug.trace extraMarble (\_ -> "")

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
                        -- removeIndex + 2
                        -- removeIndex + 1
                        if removeIndex + 2 > Array.length board_ then
                            1
                        else
                            removeIndex + 2


                    -- _ = Debug.trace (removeIndex) (\_ -> "")
                    -- _ = Debug.trace (marbleIndex_) (\_ -> "")
                    -- _ = Debug.trace (show board) (\_ -> "")
                    -- _ = Debug.trace (show board_) (\_ -> "")
                    -- _ = Debug.trace ("") (\_ -> "")

                    -- _ = Debug.trace targetMarble (\_ -> "")
                    -- _ = Debug.trace (show board) (\_ -> "")
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

                    -- _ = Debug.trace targetMarble (\_ -> "")
                    -- _ = Debug.trace (show board) (\_ -> "")
                    -- _ = if targetMarble == 47 || targetMarble == 48 then
                    --         Debug.trace (show board_) (\_ -> "")
                    --     else
                    --         ""
                in
                if Array.null remainingMarbles then
                    getWinnerScore players_
                else
                    go board_ marbleIndex_ marbleIndex playerIndex_ players_ remainingMarbles_

-- 421237 ??? -- Too low
-- 420721
-- 434729 ??? -- Too high
-- 425688
firstChallenge :: Int -> Int -> Int
firstChallenge playersMax marblesMax =
    let
        players =
            Map.fromFoldable
                $ map (\index -> Tuple index 0)
                -- $ Array.range 1 411
                -- $ Array.range 1 9
                -- $ Array.range 1 10
                -- $ Array.range 1 13
                -- $ Array.range 1 17
                -- $ Array.range 1 21
                -- $ Array.range 1 30
                $ Array.range 1 playersMax

        marbles =
            -- Array.range 1 71170
            -- Array.range 1 25
            -- Array.range 1 1618
            -- Array.range 1 7999
            -- Array.range 1 1104
            -- Array.range 1 6111
            -- Array.range 1 5807
            Array.range 1 marblesMax
    in
    solve_ players marbles

-- 1561260
secondChallenge :: Int -> Int -> Int
secondChallenge playersMax marblesMax =
    let
        players =
            Map.fromFoldable
                $ map (\index -> Tuple index 0)
                -- $ Array.range 1 411
                -- $ Array.range 1 9
                -- $ Array.range 1 10
                -- $ Array.range 1 13
                -- $ Array.range 1 17
                -- $ Array.range 1 21
                -- $ Array.range 1 30
                $ Array.range 1 playersMax

        marbles =
            -- Array.range 1 71170
            -- Array.range 1 25
            -- Array.range 1 1618
            -- Array.range 1 7999
            -- Array.range 1 1104
            -- Array.range 1 6111
            -- Array.range 1 5807
            Array.range 1 marblesMax
    in
    solve_ players marbles
