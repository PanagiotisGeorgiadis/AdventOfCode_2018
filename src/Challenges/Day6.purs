module Challenges.Day6 where


import Prelude

import Data.Array as Array
import Data.EuclideanRing (mod)
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (abs)
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

type Coordinates = Tuple Int Int

type CoordinatesObj =
    { id :: Int
    , coords :: Coordinates
    }

type Grid = Map Coordinates Int


maxRowWidth :: Int
maxRowWidth = 500

maxManhatanDistance :: Int
maxManhatanDistance = 10000


getInitialGrid :: Grid
getInitialGrid =
    Map.fromFoldable
        $ Array.concat
        $ map
            (\x ->
                map
                    (\y ->
                        (Tuple (Tuple x y) 0)
                    )
                    (Array.range 0 (maxRowWidth - 1))
            )
            (Array.range 0 (maxRowWidth - 1))


getManhatanDistance :: Coordinates -> Coordinates -> Int
getManhatanDistance (Tuple p1 p2) (Tuple q1 q2) =
    (abs (p1 - q1)) + (abs (p2 - q2))


parseCoords :: String -> Coordinates
parseCoords coordStr =
    let
        coordArr =
            String.split (String.Pattern ", ") coordStr

        parseInt =
            Maybe.fromMaybe 0 <<< Int.fromString
    in
    case coordArr of
        [ y, x ] ->
            Tuple (parseInt x) (parseInt y)
        _ ->
            Tuple 0 0


toCoordObject :: Tuple Int Coordinates -> CoordinatesObj
toCoordObject (Tuple id coords) =
    { id : id
    , coords : coords
    }


isDuplicateDistance :: { id :: Int, distance :: Int } -> Array { id :: Int, distance :: Int } -> Boolean
isDuplicateDistance targetItem distances =
    let
        sameDistances =
            Array.filter ((==) targetItem.distance <<< _.distance) distances
    in
    if Array.length sameDistances == 1 then
        false
    else
        true


drawCoordinates :: Array CoordinatesObj -> Grid
drawCoordinates coordObjs =
    let
        initialGrid =
            getInitialGrid

        updatedGrid =
            mapWithIndex
                (\gridCoords value ->
                    let
                        manhatanDistances =
                            map
                                (\item ->
                                    { id : item.id
                                    , distance : getManhatanDistance gridCoords item.coords
                                    }
                                )
                                coordObjs

                        closestElement =
                            Array.foldl
                                (\res item ->
                                    if res.distance > item.distance then
                                        item
                                    else
                                        res
                                )
                                { id : 0, distance : getManhatanDistance (Tuple 0 0) (Tuple maxRowWidth maxRowWidth) }
                                manhatanDistances
                    in
                    if isDuplicateDistance closestElement manhatanDistances then
                        0
                    else
                        closestElement.id
                )
                initialGrid
    in
    updatedGrid


gridEdges :: Array Coordinates
gridEdges =
    let
        coordRange start =
            (Array.range start (maxRowWidth - 1))

        maxRowWidth_ =
            maxRowWidth - 1

        firstRow =
            map (\x -> (Tuple x 0)) (coordRange 0)

        lastRow =
            map (\x -> (Tuple x maxRowWidth_)) (coordRange 0)

        firstColumn =
            map (\y -> (Tuple 0 y)) (coordRange 1)

        lastColumn =
            map (\y -> (Tuple maxRowWidth_ y)) (coordRange 1)
    in
    firstRow
        <> lastRow
        <> firstColumn
        <> lastColumn


removeInfinites :: Grid -> Array CoordinatesObj -> Array CoordinatesObj
removeInfinites grid coordObjs =
    Array.foldl
        (\objs coord ->
            let
                infiniteId =
                    Map.lookup coord grid
            in
            case infiniteId of
                Just id ->
                    Array.filter ((/=) id <<< _.id) objs

                Nothing ->
                    objs
        )
        coordObjs
        gridEdges


getGridArea :: Int -> Grid -> Int
getGridArea id grid =
    Array.length $
        Array.filter ((==) id) (Array.fromFoldable $ Map.values grid)


solve :: Array CoordinatesObj -> Int
solve coordObjs =
    let
        updatedGrid =
            drawCoordinates coordObjs

        updatedCoordObjs =
            removeInfinites updatedGrid coordObjs

        largestArea =
            Array.foldl
                (\res obj ->
                    let
                        gridArea =
                            getGridArea obj.id updatedGrid
                    in
                    if gridArea > res then
                        gridArea
                    else
                        res
                )
                0
                updatedCoordObjs
    in
    largestArea

-- 5532
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day6.txt")
    Console.log
        $ show
        $ solve
        $ map toCoordObject
        $ Array.zip (Array.range 1 100)
        $ map parseCoords
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


revealClosestToAll :: Array CoordinatesObj -> Grid -> Grid
revealClosestToAll coordObjs grid =
    mapWithIndex
        (\gridCoords value ->
            let
                manhatanDistances =
                    map
                        (\item ->
                            { id : item.id
                            , distance : getManhatanDistance gridCoords item.coords
                            }
                        )
                        coordObjs

                isCloseElement =
                    (sum $ map _.distance manhatanDistances) < maxManhatanDistance
            in
            if isCloseElement then
                1
            else
                value
        )
        grid


solve_ :: Array CoordinatesObj -> Int
solve_ coordObjs =
    let
        closestCoordsGrid =
            revealClosestToAll coordObjs getInitialGrid

        regionSize =
            getGridArea 1 closestCoordsGrid
    in
    regionSize

-- 36216
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day6.txt")
    Console.log
        $ show
        $ solve_
        $ map toCoordObject
        $ Array.zip (Array.range 1 100)
        $ map parseCoords
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
