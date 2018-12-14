module Challenges.Day11b where


import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.Map (Map(..), values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set as Set
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

-- hundredMillionArray :: Lazy (Unit -> Array Int)
-- hundredMillionArray =
--     defer (\_ -> Array.range 1 100000000)
--
-- -- takeFirst :: Lazy (Int -> Int)
-- takeFirst index =
--     defer (\_ -> Array.take index hundredMillionArray)

--
gridSerialNumber :: Int
-- gridSerialNumber = 5535
gridSerialNumber = 18
-- gridSerialNumber = 42

type Coords = Tuple Int Int


constructPowerMap :: Array Coords -> Map Coords Int
constructPowerMap =
    Map.fromFoldable <<<
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

initialGrid :: Map Coords Int
initialGrid =
    let
        range =
            Array.range 1 300

        coords =
            Array.concat
                $ map
                    (\x ->
                        map (\y -> Tuple x y) range
                    )
                    range
    in
    constructPowerMap coords


-- type SquareGrid =
--     { topLeftCoord :: Coords
--     , score :: Int
--     }
--
--
-- getSquareGrid :: Coords -> Map Coords Int -> SquareGrid
-- getSquareGrid (Tuple x y) grid =
--     let
--         keys =
--             Array.concat
--             $ map
--                 (\x ->
--                     map
--                         (\y ->
--                             Tuple x y
--                         )
--                         (Array.range y (y + 2))
--                 )
--                 (Array.range x (x + 2))
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
--     , score : score
--     }


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


-- firstChallenge :: SquareGrid
-- firstChallenge =
--     let
--         powerGrid =
--             initialGrid
--
--         squareGrids =
--             map
--                 (\key ->
--                     getSquareGrid key powerGrid
--                 )
--                 (Array.fromFoldable $ Map.keys powerGrid)
--
--         maxScore =
--             Array.foldl
--                 (\res val ->
--                     if val.score > res.score then
--                         val
--                     else
--                         res
--                 )
--                 ( { topLeftCoord : Tuple 0 0
--                   , score : 0
--                   }
--                 )
--                 squareGrids
--     in
--     maxScore

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
                             , (3,2), (3,3) (3,4) (3,5)
                             , (4,2), (4,3) (4,4) (4,5)
                            ]
-}
-- type SquareScore =
--     { size :: Int
--     , score :: Int
--     }
--
--
-- getSquareScore :: List Coords -> Map Coords Int -> SquareScore
-- getSquareScore keys grid =
--     List.foldl
--         (\square key ->
--             { size: square.size + 1
--             , score: square.score + ( Maybe.fromMaybe 0 $ Map.lookup key grid )
--             }
--         )
--         ({ size : 0, score : 0 })
--         keys


-- getSquareKeys :: Coords -> List (List Coords)
-- getSquareKeys (Tuple x y) =
--     let
--         maxSize =
--             min (300 - x) (300 - y)
--     in
--     map
--         (\size ->
--             let
--                 xs = List.range x (x + size)
--                 ys = List.range y (y + size)
--             in
--             List.concatMap (\x -> map (Tuple x) ys) xs
--         )
--         (List.range 1 maxSize)





type CustomSquareGrid =
    { topLeftCoord :: Coords
    , score :: Int
    , size :: Int
    }


-- getCustomSquareGrid :: Coords -> Map Coords Int -> CustomSquareGrid
-- getCustomSquareGrid coords grid =
--     let
--         squares =
--             getSquareKeys coords
--
--         values =
--             map
--                 (\square ->
--                     getSquareScore square grid
--                 )
--                 squares
--
--         biggestScore =
--             Array.foldl
--                 (\res val ->
--                     if val.score > res.score then
--                         val
--                     else
--                         res
--                 )
--                 ({ size : 0, score : 0 })
--                 values
--     in
--     { topLeftCoord : coords
--     , size : biggestScore.size
--     , score : biggestScore.score
--     }


-- getSquareKeys :: Coords -> List (List Coords)
-- getSquareKeys (Tuple x y) =
--     let
--         maxSize =
--             min (300 - x) (300 - y)
--     in
--     map
--         (\size ->
--             let
--                 xs = List.range x (x + size)
--                 ys = List.range y (y + size)
--             in
--             List.concatMap (\x -> map (Tuple x) ys) xs
--         )
--         (List.range 1 maxSize)

-- getBiggestSquareValue :: Coords -> Map Coords Int -> CustomSquareGrid
-- getBiggestSquareValue (Tuple x y) grid =
--     let
--         maxSize =
--             min ( 300 - x ) ( 300 - y )
--     in
--     go maxSize { topLeftCoord : Tuple 0 0, size: 0, score: 0 }
--         where go size biggestScore =
--                 let
--                     xs = List.range x (x + size)
--                     ys = List.range y (y + size)
--
--                     keys =
--                         List.concatMap (\x -> map (Tuple x) ys) xs
--
--                     score =
--                         List.foldl
--                             (\sum key ->
--                                 sum + ( Maybe.fromMaybe 0 $ Map.lookup key grid )
--                             )
--                             0
--                             keys
--                 in
--                 if size == 0 then
--                     biggestScore
--                 else if score > biggestScore.score then
--                     go (size - 1)
--                         { topLeftCoord : Tuple x y
--                         , size : size
--                         , score : score
--                         }
--                 else
--                     go (size - 1) biggestScore



getSum :: Array Coords -> Map Coords Int -> Int
getSum coords grid =
    Array.foldl
        (\sum key ->
            sum + (Maybe.fromMaybe 0 $ Map.lookup key grid)
        )
        0
        coords


getCoordsToSubtract :: Coords -> Array Coords
getCoordsToSubtract (Tuple x y) =
    Array.filter
        (\coord ->
            Tuple.fst coord > 0 && Tuple.snd coord > 0
        )
    $ map
        (\n -> Tuple (x - n) (y - n))
        (Array.range 1 (max x y))


getPreviousCoords :: Coords -> Array Coords -> Array Coords
getPreviousCoords (Tuple x y) excludingCoords =
    Array.filter
        (\coord ->
            if Array.null excludingCoords then
                true
            else
                List.any ((/=) coord) excludingCoords
        )
    $ Array.filter ((/=) (Tuple x y))
    $ Array.concat
    $ map
        (\x_ ->
            map (Tuple x_) (Array.range 1 y)
        )
        (Array.range 1 x)


createSummedAreaTable :: Map Coords Int -> Map Coords Int
createSummedAreaTable grid =
    mapWithIndex
        (\coords val ->
            let
                coordsToSubtract =
                    getCoordsToSubtract coords

                coordsToAdd =
                    -- getPreviousCoords (Tuple x y) coordsToSubtract
                    getPreviousCoords coords []

                addValues =
                    getSum coordsToAdd grid

                subtractValues =
                    getSum coordsToSubtract grid
            in
            val + addValues
            -- val + addValues - subtractValues
        )
        grid


initialGrid_ :: Map Coords Int
initialGrid_ =
    Map.fromFoldable
        [ Tuple (Tuple 1 1) 31
        , Tuple (Tuple 1 2) 2
        , Tuple (Tuple 1 3) 4
        , Tuple (Tuple 1 4) 33
        , Tuple (Tuple 1 5) 5
        , Tuple (Tuple 1 6) 36
        , Tuple (Tuple 2 1) 12
        , Tuple (Tuple 2 2) 26
        , Tuple (Tuple 2 3) 9
        , Tuple (Tuple 2 4) 10
        , Tuple (Tuple 2 5) 29
        , Tuple (Tuple 2 6) 25
        , Tuple (Tuple 3 1) 13
        , Tuple (Tuple 3 2) 17
        , Tuple (Tuple 3 3) 21
        , Tuple (Tuple 3 4) 22
        , Tuple (Tuple 3 5) 20
        , Tuple (Tuple 3 6) 18
        , Tuple (Tuple 4 1) 24
        , Tuple (Tuple 4 2) 23
        , Tuple (Tuple 4 3) 15
        , Tuple (Tuple 4 4) 16
        , Tuple (Tuple 4 5) 14
        , Tuple (Tuple 4 6) 19
        , Tuple (Tuple 5 1) 30
        , Tuple (Tuple 5 2) 8
        , Tuple (Tuple 5 3) 28
        , Tuple (Tuple 5 4) 27
        , Tuple (Tuple 5 5) 11
        , Tuple (Tuple 5 6) 7
        , Tuple (Tuple 6 1) 1
        , Tuple (Tuple 6 2) 35
        , Tuple (Tuple 6 3) 34
        , Tuple (Tuple 6 4) 3
        , Tuple (Tuple 6 5) 32
        , Tuple (Tuple 6 6) 6
        ]

{-
    3 2  -> x           y
    3 5  -> x          (y + size)
    6 2  -> (x + size) y
    6 5  -> (x + size) (y + size)
-}
getCustomSquareGridKeys :: Coords -> Int -> Array Coords
getCustomSquareGridKeys (Tuple x y) size =
    [ Tuple x          y
    , Tuple x          (y + size)
    , Tuple (x + size) y
    , Tuple (x + size) (y + size)
    ]


getCustomSquareGrid :: Coords -> Map Coords Int -> CustomSquareGrid
getCustomSquareGrid (Tuple x y) grid =
    let
        maxSize =
            min (300 - x) (300 - y)

        keys =
            map (getCustomSquareGridKeys (Tuple x y)) (Array.range 1 maxSize)

        biggestGrid =
            Array.foldl
                (\res keySet ->
                    case keySet of
                        [ a, b, c, d ] ->
                            let
                                getMapValue k =
                                    Maybe.fromMaybe 0
                                        $ Map.lookup k grid

                                squareScore =
                                    getMapValue d
                                        + getMapValue a
                                        - getMapValue b
                                        - getMapValue c
                            in
                            if squareScore > res.score then
                                { score : squareScore
                                , size : Tuple.fst d - Tuple.fst a
                                }
                            else
                                res
                        _ ->
                            ({ score : 0, size : 0 })
                )
                ({ score : 0, size : 0 })
                keys
    in
    { topLeftCoord : Tuple x y
    , score : biggestGrid.score
    , size : biggestGrid.size
    }

secondChallenge :: Effect Unit
secondChallenge =
    let
        -- powerGrid =
        --     initialGrid
        summedTable =
            -- createSummedAreaTable initialGrid_
            createSummedAreaTable initialGrid

        biggestGrid =
            Array.foldl
                (\res key ->
                    let
                        square =
                            getCustomSquareGrid key summedTable
                    in
                    if square.score > res.score then
                        square
                    else
                        res
                )
                { topLeftCoord : Tuple 0 0
                , score : 0
                , size : 0
                }
                (Map.keys summedTable)

        -- _ = Debug.trace customGrids (\_ -> "")

        str =
            Array.foldl
                (\res value ->
                    if mod (Array.length $ String.split (String.Pattern " ") res) 6 == 0 then
                        res <> " " <> show value <> "\n"
                    else
                        res <> " " <> show value
                )
                ""
                (Map.values summedTable)
    in
    Console.log $ show biggestGrid
    -- Console.log str
{-
secondChallenge :: CustomSquareGrid
secondChallenge =
    let
        powerGrid =
            initialGrid

        biggestSquares =
            Set.map
                (\key ->
                    -- getCustomSquareGrid key powerGrid
                    getBiggestSquareValue key powerGrid
                )
                (Map.keys powerGrid)
            -- getBiggestSquareValue (Tuple 1 1) powerGrid
            -- mapWithIndex
            --     (\key val ->
            --         let
            --             -- squares =
            --             --     getSquareKeys key
            --             squares =
            --                 getBiggestSquareValue key powerGrid
            --
            --             -- values =
            --             --     map
            --             --         (\square ->
            --             --             -- getSquareScore square grid
            --             --             Array.foldl
            --             --                 (\res key ->
            --             --                     { size: res.size + 1
            --             --                     , score: res.score + ( Maybe.fromMaybe 0 $ Map.lookup key grid )
            --             --                     }
            --             --                 )
            --             --                 ({ size : 0, score : 0 })
            --             --                 keys
            --             --         )
            --             --         squares
            --             --
            --             -- { size, score } =
            --             --     Array.foldl
            --             --         (\res val ->
            --             --             if val.score > res.score then
            --             --                 val
            --             --             else
            --             --                 res
            --             --         )
            --             --         ({ size : 0, score : 0 })
            --             --         values
            --         in
            --         { topLeftCoord : key
            --         , size : 0
            --         , score : 1
            --         }
            --     )
            --     powerGrid

        -- biggestSquare =
        -- --     Array.foldl
        -- --         (\res val ->
        -- --             if val.score > res.score then
        -- --                 val
        -- --             else
        -- --                 res
        -- --         )
        -- --         ( { topLeftCoord : Tuple 0 0
        -- --           , size : 0
        -- --           , score : 0
        -- --           }
        -- --         )
        -- --         squareGrids
        --     { size: 0, topLeftCoord : Tuple 0 0, score: 100 }
    in
    Array.foldl
        (\res val ->
            if val.score > res.score then
                val
            else
                res
        )
        -- ({ topLeftCoord : Tuple 0 0, size: 0, score: 0 })
        ( { topLeftCoord : Tuple 0 0
          , score : 0
          , size : 0
          }
        )
        biggestSquares
-}
