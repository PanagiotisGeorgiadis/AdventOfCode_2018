module Challenges.Day11 where


import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map (Map(..), values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
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


-- newtype Coords = Coords (Tuple Int Int)
--
-- instance showCoords :: Show Coords
--     where show (Coords obj) = show obj
--
-- instance coordsEq :: Eq Coords
--     where eq (Coords lhs) (Coords rhs) = rhs == lhs
--
-- instance coordsOrd :: Ord Coords
--     where compare (Coords lhs) (Coords rhs) = compare lhs rhs
--
--
-- newtype Point = Point
--     { position :: Coords
--     , velocity :: Coords
--     }
--
-- instance showPoint :: Show Point
--     where show (Point obj) = show obj.position
--
-- instance pointEq :: Eq Point
--     where eq (Point lhs) (Point rhs) = lhs.position == rhs.position
--
-- instance pointOrd :: Ord Point
--     where compare (Point lhs) (Point rhs) = compare lhs.position rhs.position

-- 300 x 300
-- x 1 - 300
-- y 1 - 300
-- 3x3 square with the largest power!
-- power level = (rackId * y + gridSerialNumber) * rackId
-- where rackId = x + 10, gridSerialNumber = puzzleInput.
-- Keep only the hundreds digit of the power level - 5

--
gridSerialNumber :: Int
gridSerialNumber = 5535

type Coords = Tuple Int Int

initialGrid :: Map Coords Int
initialGrid =
    let
        coords =
            Array.concat
                $ map
                    (\x ->
                        map
                            (\y ->
                                Tuple x y
                            )
                            (Array.range 1 300)
                    )
                    (Array.range 1 300)

        powerMap =
            map
                (\(Tuple x y) ->
                    let
                        rackId =
                            x + 10

                        resultStr =
                            show $ ((rackId * y) + gridSerialNumber) * rackId

                        resultLen =
                            String.length resultStr

                        value =
                            if resultLen < 3 then
                                0
                            else
                                Maybe.fromMaybe 0
                                    $ Int.fromString
                                    $ String.take 1
                                    $ String.drop (resultLen - 3) resultStr

                    in
                    Tuple (Tuple x y) value
                )
                coords
    in
    Map.fromFoldable powerMap


type SquareGrid =
    { topLeftCoord :: Coords
    , score :: Int
    }


getSquareGrid :: Coords -> Map Coords Int -> SquareGrid
getSquareGrid (Tuple x y) grid =
    let
        keys =
            Array.concat
            $ map
                (\x ->
                    map
                        (\y ->
                            Tuple x y
                        )
                        (Array.range y (y + 2))
                )
                (Array.range x (x + 2))

        values =
            map
                (\key ->
                    Map.lookup key grid
                )
                keys

        score =
            if Array.any ((==) Nothing) values then
                0
            else
                Array.foldl
                    (\res val ->
                        res + Maybe.fromMaybe 0 val
                    )
                    0
                    values
    in
    { topLeftCoord : Tuple x y
    , score : score
    }


-- toPowerGrid :: Map Coords Int -> Map Coords Int
-- toPowerGrid grid =
--     mapWithIndex
--         (\(Tuple x y) value ->
--             let
--                 rackId =
--                     x + 10
--
--                 resultStr =
--                     show $ ((rackId * y) + gridSerialNumber) * rackId
--
--                 resultLen =
--                     String.length resultStr
--             in
--             if resultLen < 3 then
--                 0
--             else
--                 Maybe.fromMaybe 0
--                     $ Int.fromString
--                     $ String.take 1
--                     $ String.drop (resultLen - 3) resultStr
--         )
--         grid


firstChallenge :: SquareGrid
firstChallenge =
    let
        powerGrid =
            initialGrid

        squareGrids =
            map
                (\key ->
                    getSquareGrid key powerGrid
                )
                (Array.fromFoldable $ Map.keys powerGrid)

        maxScore =
            Array.foldl
                (\res val ->
                    if val.score > res.score then
                        val
                    else
                        res
                )
                ( { topLeftCoord : Tuple 0 0
                  , score : 0
                  }
                )
                squareGrids
    in
    maxScore

-- type CoordsWithSize = Tuple Coords Int

-- type SizeAndValue = Tuple Int Int
--
--
-- coordsToIndex :: Coords -> Int
-- coordsToIndex (Tuple x y) =
--     (x - 1) * 300 + y
--
-- initialGrid_ :: Map Coords SizeAndValue
-- initialGrid_ =
--     let
--         range =
--             Array.range 1 300
--
--         coords =
--             Array.concat
--                 $ map
--                     (\x ->
--                         map (\y -> Tuple x y) range
--                     )
--                     range
--
--         -- coordsWithSize =
--         --     map
--         --         (\c ->
--         --             map
--         --                 (\s ->
--         --                     Tuple c s
--         --                 )
--         --                 (Array.range 1 300)
--         --         )
--         --         coords
--
--         -- _ = Debug.trace (Array.length coordsWithSize) (\_ -> "")
--
--         -- coordsWithSize =
--         --     Array.concat
--         --         $ map
--         --             (\c -> map (\s -> Tuple c s) (Array.range 1 300)) coords
--
--         powerMap =
--             map
--                 (\(Tuple x y) ->
--                     let
--                         rackId =
--                             x + 10
--
--                         resultStr =
--                             show $ ((rackId * y) + gridSerialNumber) * rackId
--
--                         resultLen =
--                             String.length resultStr
--
--                         value =
--                             if resultLen < 3 then
--                                 0
--                             else
--                                 Maybe.fromMaybe 0
--                                     $ Int.fromString
--                                     $ String.take 1
--                                     $ String.drop (resultLen - 3) resultStr
--
--                     in
--                     -- Tuple (Tuple x y) value
--                     map
--                         (\size ->
--                             Tuple (Tuple x y) (Tuple size value)
--                         )
--                         range
--                 )
--                 coords
--     in
--     -- Map.fromFoldable powerMap
--     Map.fromFoldable [ Tuple (Tuple 1 1) (Tuple 1 15) ]
--
--
-- type CustomSquareGrid =
--     { topLeftCoord :: Coords
--     , size :: Int
--     , score :: Int
--     }
--
-- getCustomSquareGrid :: Coords -> Int -> Map Coords Int -> CustomSquareGrid
-- getCustomSquareGrid (Tuple x y) size grid =
--     let
--         keys =
--             Array.concat
--                 $ map
--                     (\x ->
--                         map (\y -> Tuple x y) (Array.range y (y + size))
--                     )
--                     (Array.range x (x + size))
--
--         values =
--             map
--                 (\key ->
--                     Map.lookup key grid
--                 )
--                 keys
--
--         score =
--             if Array.any ((==) Nothing) values then
--                 0
--             else
--                 Array.foldl
--                     (\res val ->
--                         res + Maybe.fromMaybe 0 val
--                     )
--                     0
--                     values
--     in
--     { topLeftCoord : Tuple x y
--     , size : size
--     , score : score
--     }


{-
-- 1,2 -> min = 5 -2 = 3 === [ (1,2), (1,3) (1,4) (1,5)
                             , (2,2), (2,3) (2,4) (2,5)
                             , (3,2), (3,)
                            ]
-}


getSquareScore :: Array Coords -> Map Coords Int -> Tuple Int Int
getSquareScore keys grid =
    Array.foldl
        (\res key ->
            let
                score =
                    Tuple.snd res + (Maybe.fromMaybe 0 $ Map.lookup key grid)

                size =
                    Tuple.fst res + 1
            in
            Tuple score size
        )
        (Tuple 0 0)
        keys


getSquareKeys :: Coords -> Array (Array Coords)
getSquareKeys (Tuple x y) =
    let
        maxSize =
            min (300 - x) (300 - y)
    in
    -- Array.concat
    map
        (\size ->
            let
                xs = Array.range x (x + size)
                ys = Array.range y (y + size)
            in
            Array.concat
                $ map (\x -> map (Tuple x) ys) xs
        )
        (Array.range 1 maxSize)


type CustomSquareGrid =
    { topLeftCoord :: Coords
    , score :: Int
    , size :: Int
    }


getCustomSquareGrid :: Coords -> Map Coords Int -> CustomSquareGrid
getCustomSquareGrid (Tuple x y) grid =
    let
        squares =
            getSquareKeys (Tuple x y)

        values =
            map
                (\square ->
                    getSquareScore square grid
                )
                squares

        biggestScore =
            Array.foldl
                (\res val ->
                    if Tuple.snd val > Tuple.snd res then
                        val
                    else
                        res
                )
                (Tuple 0 0)
                values
    in
    { topLeftCoord : Tuple x y
    , size : Tuple.fst biggestScore
    , score : Tuple.snd biggestScore
    }

secondChallenge :: CustomSquareGrid
secondChallenge =
    let
        powerGrid =
            initialGrid

        squareGrids =
            map
                (\key ->
                    getCustomSquareGrid key powerGrid
                )
                (Array.fromFoldable $ Map.keys powerGrid)

        biggestSquare =
            Array.foldl
                (\res val ->
                    if val.score > res.score then
                        val
                    else
                        res
                )
                ( { topLeftCoord : Tuple 0 0
                  , size : 0
                  , score : 0
                  }
                )
                squareGrids
    in
    biggestSquare
