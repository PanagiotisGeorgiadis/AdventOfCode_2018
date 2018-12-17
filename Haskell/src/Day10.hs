{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Prelude

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
    deriving Show


data Cell = PointCell [Coords] | EmptyCell

instance Show Cell
    where show (PointCell _) = "#"
          show EmptyCell     = "."


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
            map (getDigits . Text.unpack . Text.strip)
                $ Text.splitOn ","
                $ Text.replace "velocity=<" ""
                $ Text.strip
                $ head
                $ drop 1
                $ Text.splitOn ">" line
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


performTick :: [Point] -> [Point]
performTick =
    map
        (\(Point coords vel) ->
            Point
                ( fst coords + fst vel
                , snd coords + snd vel
                )
                vel
        )


shrinkPoints :: Int -> [Point] -> [Point]
shrinkPoints prevRowSize points =
    let
        updatedPoints = performTick points
           {- map
                (\(Point coords vel) ->
                    Point
                        ( fst coords + fst vel
                        , snd coords + snd vel
                        )
                        vel
                )
                points
           -}

        rowSize = getRowSize updatedPoints
    in
    if prevRowSize < rowSize then
        points
    else
        shrinkPoints rowSize updatedPoints


shrinkPointsTime :: Int -> [Point] -> Int
shrinkPointsTime size = go 0 size
    where go secs prevRowSize points =
            let
                updatedPoints = performTick points
                rowSize       = getRowSize updatedPoints
            in
            if prevRowSize < rowSize then
                secs
            else
                go (secs + 1) rowSize updatedPoints


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


trimRowPadding :: String -> String
trimRowPadding =
    snd
    . foldl'
        (\(isValid, res) char ->
            if isValid || char == '#' then
                ( True
                , res ++ [ char ]
                )
            else
                ( isValid
                , res
                )
        )
        (False, "")


drawGrid :: [Point] -> Map Coords Cell -> String
drawGrid points grid =
    let
        rowSize_ = getRowSize points
    in
    unlines
    $ map (trimRowPadding)
    $ filter (any ((==) '#'))
    $ lines
    $ foldl'
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


firstChallenge :: IO ()
firstChallenge = do
    contents <- readFile "PuzzleInputs/Day10.txt"
    putStrLn
        $ (\points -> drawGrid points (initialGrid points))
        $ (\points -> shrinkPoints (getRowSize points) points)
        $ map parsePoint
        $ Text.lines
        $ Text.pack contents


secondChallenge :: IO () 
secondChallenge = do
    contents <- readFile "PuzzleInputs/Day10.txt"
    putStrLn
        $ show
        $ (\points -> shrinkPointsTime (getRowSize points) points) 
        $ map parsePoint
        $ Text.lines
        $ Text.pack contents


