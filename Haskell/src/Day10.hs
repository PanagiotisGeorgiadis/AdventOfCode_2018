{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Prelude

-- import Data.Array as Array
-- import Data.Foldable (sum)
-- import Data.FunctorWithIndex (mapWithIndex)
-- import Data.Int as Int
-- import Data.Map (Map(..), values)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..))
-- import Data.Maybe as Maybe
-- import Data.Ord (abs)
-- import Data.String as String
-- import Data.Tuple (Tuple(..))
-- import Data.Tuple as Tuple
-- import Debug.Trace as Debug
-- import Effect (Effect)
-- import Effect.Console as Console
-- import Effect.Exception (try)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)
-- import Utils.Helpers (getInputLines)
-- import Utils.String as StringUtils

-- import qualified Data.Sequence as Seq
import qualified Data.Maybe    as Maybe

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map

import Data.Foldable  (foldl', toList)
import Data.Sequence  (Seq)
import Data.Maybe     (Maybe(..))
import Data.Char      (digitToInt)
import Data.Text      (Text)
import Text.Read      (readMaybe)
import Data.Text.Read (decimal)
import Data.Either    (fromRight)
import Data.Map       (Map)

type Coords = (Int, Int)

data Point = Point Coords Coords
    -- { position :: Coords
    -- , velocity :: Coords
    -- } deriving Show
    deriving Show

-- instance showPoint :: Show Point
--     where show (Point obj) = show obj
--
-- instance pointEq :: Eq Point
--     where eq (Point lhs) (Point rhs) = lhs.position == rhs.position
--
-- instance pointOrd :: Ord Point
--     where compare (Point lhs) (Point rhs) = compare lhs.position rhs.position


data Cell = PointCell [Coords] | EmptyCell

instance Show Cell
    where show (PointCell _) = "#"
          show EmptyCell     = "."
--
-- instance cellEq :: Eq Cell
--     where eq (PointCell lhs) (PointCell rhs) = lhs == rhs
--           eq (PointCell lhs) EmptyCell       = false
--           eq EmptyCell       (PointCell rhs) = false
--           eq EmptyCell       EmptyCell       = true

getDigits :: String -> Int
getDigits (x:xs)
    | x == '-'  = negate $ fst $ fromRight (0,"") $ decimal $ Text.pack xs
    | otherwise = fst $ fromRight (0,"") $ decimal $ Text.pack (x:xs)

parsePoint :: Text -> Point
parsePoint line =
    let
        positions =
            map (getDigits . Text.unpack . Text.strip)
                $ Text.splitOn ","
                $ head
                $ drop 1
                $ Text.splitOn "<"
                $ head
                $ Text.splitOn ">" line

        velocities =
            -- map (fst . fromRight (0, "") . decimal . Text.strip)
            map (getDigits . Text.unpack . Text.strip)
                $ Text.splitOn ","
                $ Text.replace "velocity=<" ""
                $ Text.strip
                $ head
                $ drop 1
                $ Text.splitOn ">" line

        -- positions  = [1, 2]
        -- velocities = [3, 4]
    in
    Point
        ( head (reverse positions),  head positions )
        ( head (reverse velocities), head velocities )

getMinX :: [Point] -> Int
getMinX =
    foldl'
        (\res (Point position _) ->
            if fst position < res then
                fst position
            else
                res
        )
        0

getMaxX :: [Point] -> Int
getMaxX =
    foldl'
        (\res (Point position _) ->
            if fst position > res then
                fst position
            else
                res
        )
        0


getMinY :: [Point] -> Int
getMinY =
    foldl'
        (\res (Point position _) ->
            if snd position < res then
                snd position
            else
                res
        )
        0


getMaxY :: [Point] -> Int
getMaxY =
    foldl'
        (\res (Point position _) ->
            if snd position > res then
                snd position
            else
                res
        )
        0

getRowSize :: [Point] -> Int
getRowSize points =
    let
        minY = getMinY points
        maxY = getMaxY points
    in
    (abs maxY) + (abs minY)

initialGrid :: [Point] -> Map Coords Cell
initialGrid points =
    let
        minX = getMinX points
        minY = getMinY points
        maxX = getMaxX points
        maxY = getMaxY points

        xs = [minX..maxX]
        ys = [minY..maxY]

        grid =
            Map.fromList
                $ map (\coords -> (coords, EmptyCell))
                $ concat
                $ map (\x -> map ((,) x) ys) xs

        gridWithValues =
            foldl'
                (\res (Point position velocity) ->
                    Map.update (\v -> Just ( PointCell [velocity] )) position res
                )
                grid
                points
    in
    gridWithValues


drawGrid :: Int -> Map Coords Cell -> String
drawGrid rowSize_ grid =
    foldl'
        (\res val ->
            let
                updatedRes =
                    res <> val
            in
            -- Nasty console printing hack. + 2 is +1 because of the \n
            -- and the other one because there is a zero in between that we are not counting ?
            if mod (length updatedRes) (rowSize_ + 2) == 0 then
                updatedRes <> "\n"
            else
                updatedRes
        )
        "\n"
        (map show $ Map.elems grid)


getPointCoords :: Map Coords Cell -> [Coords]
getPointCoords grid =
    concat
        $ map
            (\key ->
                case Map.lookup key grid of
                    Just (PointCell velocities) ->
                        replicate (length velocities) key
                    _ ->
                        [key]
            )
        $ Map.keys
        $ Map.mapMaybe
            (\val ->
                case val of
                    PointCell coords -> Just coords
                    EmptyCell        -> Nothing
            )
            grid

getPointValues :: Map Coords Cell -> [Cell]
getPointValues grid =
    concat
        $ map
            (\val ->
                case val of
                    PointCell coords ->
                        map (\c -> PointCell [c]) coords

                    _ ->
                        [ val ]
            )
        $ Map.elems
        $ Map.mapMaybe
            (\val ->
                case val of
                    PointCell coords -> Just (PointCell coords)
                    EmptyCell        -> Nothing
            )
            grid


emptyCells :: [Coords] -> Map Coords Cell -> Map Coords Cell
emptyCells keys grid =
    foldl'
        (\res key ->
            Map.update (\v -> Just EmptyCell) key res
        )
        grid
        keys


keepUniques :: [Coords] -> [Coords]
keepUniques =
    foldl'
        (\res cell ->
            if any ((==) cell) res then
                res
            else
                res ++ [ cell ]
        )
        []


getVelocities :: Cell -> [Coords]
getVelocities (PointCell velocities) = velocities
-- getVelocities (EmptyCell)            = Debug.trace "Error on getVelocities" (\_ -> [])
getVelocities (EmptyCell)            = []


updatePointCells :: [Coords] -> [Cell] -> Map Coords Cell -> Map Coords Cell
updatePointCells keys cells grid =
    foldl'
        (\res (key, cell) ->
            Map.update
                (\val ->
                    case (val, cell) of
                        (PointCell vc, PointCell cc) ->
                            Just (PointCell (keepUniques (cc ++ vc)))

                        _ ->
                            Just cell
                )
                key
                res
        )
        grid
        (zip keys cells)


performTick :: Map Coords Cell -> Map Coords Cell
performTick grid =
    let
        sourceCoords = getPointCoords grid
        pointCells = getPointValues grid

        destCoords =
            zipWith
                (\coords cell ->
                    case cell of
                        PointCell coords_ ->
                            let
                                (x, y) = head coords_
                            in
                            (fst coords + x, snd coords + y)

                        EmptyCell ->
                            -- let
                            --    _ = Debug.trace "Error while getting new Coords!" (\_ -> "")
                            -- in
                            coords
                )
                sourceCoords
                pointCells
    in
    updatePointCells destCoords pointCells
        $ emptyCells sourceCoords grid


drawPositions :: [Point] -> String
drawPositions points =
    let
        rowSize_ = getRowSize points
        initialGrid_ = initialGrid points
    in
    drawGrid 21 (performTick $ performTick $ performTick initialGrid_)


solve :: Map Coords Cell -> String
solve grid =
    let
        points = map (\c -> Point c (0, 0)) $ getPointCoords grid
        -- xDiff = (getMaxX points) - (getMinX points) + 1
        -- yDiff = (getMaxY points) - (getMinY points) + 1

    {-
    in go xDiff yDiff grid
    where go prevXDiff prevYDiff grid_ =
            let
                updatedGrid = performTick grid_
                points = map (\c -> Point c (0, 0)) $ getPointCoords updatedGrid
                nextXDiff = (getMaxX points) - (getMinX points) + 1
                nextYDiff = (getMaxY points) - (getMinY points) + 1
            in
            if nextXDiff > prevXDiff && nextYDiff > prevYDiff then
                -- drawGrid 21 grid_
                drawGrid 120 grid_
            else
                go nextXDiff nextYDiff updatedGrid
    -}
    in go grid
    where go grid_ =
            let
                updatedGrid = performTick grid_
                points = map (\c -> Point c (0, 0)) $ getPointCoords updatedGrid
                rowSize = getRowSize points
            in
            if rowSize < 300 then
                drawGrid 300 grid_
            else
                go updatedGrid


firstChallenge :: IO ()
firstChallenge = do
    contents <- readFile "PuzzleInputs/Day10.txt"
    putStrLn
        -- $ show
        -- $ drawPositions
        $ solve
        $ initialGrid
        $ map parsePoint
        $ Text.lines
        $ Text.pack contents

    -- contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day10.txt")
    -- Console.log
        -- $ show
        -- $ (\_ -> show "")
        -- $ show
        -- $ drawPositions
        -- $ solve
        -- $ drawGrid 120
        --
        -- $ (\_ -> "")
        -- $ initialGrid
        -- $ map parsePoint
        -- $ map (StringUtils.removeAll "\r")
        -- $ getInputLines contents
    -- $ map parsePoint
    -- $ lines


secondChallenge :: String
secondChallenge =
    -- contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day10.txt")
    -- Console.log
    --     $ show
    --     $ map (StringUtils.removeAll "\r")
    --     $ getInputLines contents
    ""
