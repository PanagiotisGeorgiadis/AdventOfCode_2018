module Challenges.Day10b where


import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
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


-- newtype Node = Node
--     { metadata :: Array Int
--     , children :: Array Node
--     }
--
-- instance nodeShow :: Show Node
--     where show (Node obj) = show obj

type Coords = Tuple Int Int

-- instance showCoords :: Show Coords
--     where show (Coords obj) = show obj
--
-- instance coordsEq :: Eq Coords
--     where eq (Coords lhs) (Coords rhs) = rhs == lhs
--
-- instance coordsOrd :: Ord Coords
--     where compare (Coords lhs) (Coords rhs) = compare lhs rhs


newtype Point = Point
    { position :: Coords
    , velocity :: Coords
    }

instance showPoint :: Show Point
    where show (Point obj) = show obj

instance pointEq :: Eq Point
    where eq (Point lhs) (Point rhs) = lhs.position == rhs.position

instance pointOrd :: Ord Point
    where compare (Point lhs) (Point rhs) = compare lhs.position rhs.position


data Cell = PointCell (Array Coords) | EmptyCell

instance showCell :: Show Cell
    where show (PointCell _) = "#"
          show EmptyCell     = "."

instance cellEq :: Eq Cell
    where eq (PointCell lhs) (PointCell rhs) = lhs == rhs
          eq (PointCell lhs) EmptyCell       = false
          eq EmptyCell       (PointCell rhs) = false
          eq EmptyCell       EmptyCell       = true


parsePoint :: String -> Point
parsePoint line =
    let
        positions =
            map (Maybe.fromMaybe 0 <<< Int.fromString <<< String.trim)
            $ String.split (String.Pattern ",")
            $ Maybe.fromMaybe ""
            $ Array.head
            $ Array.drop 1
            $ String.split (String.Pattern "<")
            $ Maybe.fromMaybe ""
            $ Array.head
            $ String.split (String.Pattern ">") line

        velocities =
            map (Maybe.fromMaybe 0 <<< Int.fromString <<< String.trim)
            $ String.split (String.Pattern ",")
            $ StringUtils.removeAll "velocity=<"
            $ String.trim
            $ Maybe.fromMaybe ""
            $ Array.head
            $ Array.drop 1
            $ String.split (String.Pattern ">") line

        getInt =
            Maybe.fromMaybe 0 <<< Array.head
    in
    Point
        { position : Tuple (getInt $ Array.reverse positions) (getInt positions)
        , velocity : Tuple (getInt $ Array.reverse velocities) (getInt velocities)
        }


rowSize :: Int
-- rowSize = 150
rowSize = 22


-- drawBoard :: Coords -> Coords -> Map Coords String
-- drawBoard (Tuple x y) (Tuple x_ y_) =
--     let
--         xs = Array.range x x_
--         ys = Array.range y y_
--         board =
--             map
--                 (\coords ->
--                     Tuple coords "."
--                 )
--             $ Array.concat
--             $ map (\x -> map (Tuple x) ys) xs
--     in
--     Map.fromFoldable board
--
--
-- compareCustom :: Coords -> Coords -> Ordering
-- compareCustom (Tuple x y) (Tuple x_ y_) =
--     if y == y_ then
--         compare x x
--     else
--         compare y y_
--
--
-- printBoard_ :: Map Coords String -> String
-- printBoard_ board =
--     let
--         sortedKeys =
--             Array.sortBy compareCustom
--                 $ Array.fromFoldable
--                 $ Map.keys board
--             -- Array.fromFoldable $ Map.keys board
--
--         -- _ = Debug.trace sortedKeys (\_ -> "")
--
--
--         board_ =
--             -- mapWithIndex
--             --     (\(Coords (Tuple x y)) val ->
--             --         if y == 11 then
--             --             val <> "\n"
--             --         else
--             --             val
--             --     )
--             --     board
--             Array.foldl
--                 (\res (Tuple x y) ->
--                     case Map.lookup (Tuple x y) board of
--                         Just val ->
--                             if x == 15 then
--                                 res <> val <> "\n"
--                             else
--                                 res <> val
--
--                         Nothing ->
--                             res
--                 )
--                 ""
--                 sortedKeys
--
--         values =
--             -- String.joinWith ""
--             --     $ Array.fromFoldable
--             --     $ Map.values board_
--             board_
--     in
--     values
--
--
--
-- printResponse :: Array Point -> String
-- printResponse sortedPoints =
--     let
--         positions = map (\(Point p) -> p.position) sortedPoints
--
--         xs = map Tuple.fst positions
--         ys = map Tuple.snd positions
--
--         minX = Array.foldl min 0 xs
--         maxX = Array.foldl max 0 xs
--         minY = Array.foldl min 0 ys
--         maxY = Array.foldl max 0 ys
--
--         initialBoard =
--             drawBoard (Tuple minX minY) (Tuple maxX maxY)
--
--         updatedBoard =
--             Array.foldl
--                 (\res (Point point) ->
--                     Map.update (\v -> Just "#") point.position res
--                 )
--                 initialBoard
--                 -- (Map.fromFoldable (Tuple (Coords (Tuple 0 0)) ""))
--                 sortedPoints
--
--         boardString =
--             printBoard_ updatedBoard
--     in
--     boardString

getMinX :: Array Point -> Int
getMinX =
    Array.foldl
        (\res (Point { position }) ->
            if Tuple.fst position < res then
                Tuple.fst position
            else
                res
        )
        0

getMaxX :: Array Point -> Int
getMaxX =
    Array.foldl
        (\res (Point { position }) ->
            if Tuple.fst position > res then
                Tuple.fst position
            else
                res
        )
        0


getMinY :: Array Point -> Int
getMinY =
    Array.foldl
        (\res (Point { position }) ->
            if Tuple.snd position < res then
                Tuple.snd position
            else
                res
        )
        0


getMaxY :: Array Point -> Int
getMaxY =
    Array.foldl
        (\res (Point { position }) ->
            if Tuple.snd position > res then
                Tuple.snd position
            else
                res
        )
        0

getRowSize :: Array Point -> Int
getRowSize points =
    let
        minY = getMinY points
        maxY = getMaxY points
    in
    (abs maxY) + (abs minY)

initialGrid :: Array Point -> Map Coords Cell
initialGrid ps =
    let
        points =
            map
                (\(Point { position, velocity }) ->
                    Point
                        { position : Tuple (Tuple.fst position / 10) (Tuple.snd position / 10)
                        , velocity : velocity
                        }
                )
                ps

        minX = getMinX points
        minY = getMinY points
        maxX = getMaxX points
        maxY = getMaxY points

        xs = Array.range minX maxX
        ys = Array.range minY maxY

        grid =
            Map.fromFoldable
                $ map (\coords -> Tuple coords EmptyCell)
                    $ Array.concat
                    $ map (\x -> map (Tuple x) ys) xs

        gridWithValues =
            Array.foldl
                (\res (Point point) ->
                    Map.update (\v -> Just (PointCell [point.velocity])) point.position res
                )
                grid
                points
    in
    gridWithValues


drawGrid :: Int -> Map Coords Cell -> String
drawGrid rowSize_ grid =
    Array.foldl
        (\res val ->
            let
                updatedRes =
                    res <> val
            in
            -- Nasty console printing hack. + 2 is +1 because of the \n
            -- and the other one because there is a zero in between that we are not counting ?
            if mod (String.length updatedRes) (rowSize_ + 2) == 0 then
                updatedRes <> "\n"
            else
                updatedRes
        )
        "\n"
        (Array.fromFoldable $ map show $ Map.values grid)


getPointCoords :: Map Coords Cell -> Array Coords
getPointCoords grid =
    Array.concat
        $ map
            (\key ->
                case Map.lookup key grid of
                    Just (PointCell coords) ->
                        Array.replicate (Array.length coords) key
                    _ ->
                        [key]
            )
        $ Array.fromFoldable
        $ Map.keys
        $ Map.mapMaybe
            (\val ->
                case val of
                    PointCell coords -> Just coords
                    EmptyCell        -> Nothing
            )
            grid

getPointValues :: Map Coords Cell -> Array Cell
getPointValues grid =
    Array.concat
        $ map
            (\val ->
                case val of
                    PointCell coords ->
                        map (\c -> PointCell [c]) coords

                    _ ->
                        [val]
            )
        $ Array.fromFoldable
        $ Map.values
        $ Map.mapMaybe
            (\val ->
                case val of
                    PointCell coords -> Just (PointCell coords)
                    EmptyCell        -> Nothing
            )
            grid


emptyCells :: Array Coords -> Map Coords Cell -> Map Coords Cell
emptyCells keys grid =
    Array.foldl
        (\res key ->
            Map.update (\v -> Just EmptyCell) key res
        )
        grid
        keys


keepUniques :: Array Coords -> Array Coords
keepUniques =
    Array.foldl
        (\res cell ->
            if Array.any ((==) cell) res then
                res
            else
                Array.snoc res cell
        )
        []


getVelocities :: Cell -> Array Coords
getVelocities (PointCell velocities) = velocities
getVelocities (EmptyCell)            = Debug.trace "Error on getVelocities" (\_ -> [])

updatePointCells :: Array Coords -> Array Cell -> Map Coords Cell -> Map Coords Cell
updatePointCells keys cells grid =
    Array.foldl
        (\res (Tuple key cell) ->
            Map.update
                (\val ->
                    case val, cell of
                        PointCell vc, PointCell cc ->
                            Just (PointCell (keepUniques (cc <> vc)))

                        _, _ ->
                            Just cell
                )
                key
                res
        )
        grid
        (Array.zip keys cells)


performTick :: Map Coords Cell -> Map Coords Cell
performTick grid =
    let
        sourceCoords =
            getPointCoords grid

        pointCells =
            getPointValues grid

        destCoords =
            Array.zipWith
                (\coords cell ->
                    case cell of
                        PointCell coords_ ->
                            case Array.head coords_ of
                                Just (Tuple x y) ->
                                    Tuple (Tuple.fst coords + x) (Tuple.snd coords + y)

                                Nothing ->
                                    coords

                        EmptyCell ->
                            let
                                _ = Debug.trace "Error while getting new Coords!" (\_ -> "")
                            in
                            coords
                )
                sourceCoords
                pointCells
    in
    updatePointCells destCoords pointCells
        $ emptyCells sourceCoords grid



drawPositions :: Array Point -> String
drawPositions points =
    let
        rowSize_ =
            getRowSize points

        initialGrid_ =
            initialGrid points
    in
    drawGrid rowSize_ (performTick $ performTick $ performTick $ performTick initialGrid_)

solve :: Map Coords Cell -> String
solve grid =
    let
        points =
            map
                (\c -> Point { position : c, velocity : Tuple 0 0 })
                $ getPointCoords grid

        xDiff = (getMaxX points) - (getMinX points) + 1
        yDiff = (getMaxY points) - (getMinY points) + 1

    in go xDiff yDiff grid
    where go prevXDiff prevYDiff grid_ =
            let
                updatedGrid =
                    performTick grid_

                points =
                    map (\c -> Point { position : c, velocity : Tuple 0 0 })
                        $ getPointCoords updatedGrid

                nextXDiff = (getMaxX points) - (getMinX points) + 1
                nextYDiff = (getMaxY points) - (getMinY points) + 1
            in
            if nextXDiff > prevXDiff && nextYDiff > prevYDiff then
                drawGrid 21 grid_
            else
                go nextXDiff nextYDiff updatedGrid


firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day10.txt")
    Console.log
        -- $ show
        -- $ (\_ -> show "")
        -- $ show
        -- $ drawPositions
        -- $ solve
        -- $ drawGrid 120
        $ (\_ -> "")
        $ initialGrid
        $ map parsePoint
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day10.txt")
    Console.log
        $ show
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
