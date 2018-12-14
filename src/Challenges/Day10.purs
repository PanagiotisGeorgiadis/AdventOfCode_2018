module Challenges.Day10 where


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


-- newtype Node = Node
--     { metadata :: Array Int
--     , children :: Array Node
--     }
--
-- instance nodeShow :: Show Node
--     where show (Node obj) = show obj

newtype Coords = Coords (Tuple Int Int)

instance showCoords :: Show Coords
    where show (Coords obj) = show obj

instance coordsEq :: Eq Coords
    where eq (Coords lhs) (Coords rhs) = rhs == lhs

instance coordsOrd :: Ord Coords
    where compare (Coords lhs) (Coords rhs) = compare lhs rhs


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
        { position : Coords (Tuple (getInt positions) (getInt $ Array.reverse positions))
        , velocity : Coords (Tuple (getInt velocities) (getInt $ Array.reverse velocities))
        }


-- applyTick :: Array Point -> Array Point
-- applyTick =
--     map
--         (\(Point { position, velocity }) ->
--             let
--                 updatedPosition =
--                     Tuple
--                         (Tuple.fst position + Tuple.fst velocity)
--                         (Tuple.snd position + Tuple.snd velocity)
--             in
--             Point
--                 { position : updatedPosition
--                 , velocity : velocity
--                 }
--         )


-- getMinCoords :: Array Point -> Coords
-- getMinCoords =
--     Array.foldl
--         (\res (Point point) ->
--             min res point.position
--         )
--         (Tuple 1000000 1000000)
--
--
-- getMaxCoords :: Array Point -> Coords
-- getMaxCoords =
--     Array.foldl
--         (\res (Point point) ->
--             max res point.position
--         )
--         (Tuple (-1000000) (-1000000))


rowSize :: Int
-- rowSize = 150
rowSize = 22

-- coordsToIndex :: Tuple Int Int -> Int
-- coordsToIndex (Tuple x y) = (x * rowSize) + y + 1

--
-- printBoard :: Array Point -> Coords -> Coords -> String
-- printBoard sortedPoints min max =
--         let
--             minX = Tuple.fst min - 50
--             minY = Tuple.snd min - 50
--             -- maxX = Tuple.fst max + 50
--             -- maxY = Tuple.snd max + 50
--
--             initialIndex =
--                 coordsToIndex (Tuple minX minY)
--         in
--         -- go "" (Tuple.fst min - 50) (Tuple.snd min - 50) (Tuple.fst max + 50) (Tuple.fst min + 50)
--         go "" initialIndex sortedPoints
--             where go board index points =
--                     let
--                         targetIndex =
--                             case Array.head points of
--                                 Just (Point point) ->
--                                     coordsToIndex point.position
--
--                                 Nothing ->
--                                     0
--
--                         updatedPoints =
--                             if index == targetIndex then
--                                 Array.drop 1 points
--                             else
--                                 points
--
--                         updatedIndex =
--                             index + 1
--
--                         board_ =
--                             if index == targetIndex then
--                                 board <> "#"
--                             else
--                                 board <> "."
--
--                         updatedBoard =
--                             if mod index rowSize == 0 then
--                                 board_ <> "\n"
--                             else
--                                 board_
--                     in
--                     if Array.null points then
--                         -- let
--                         --     emptyLine =
--                         --         ( String.joinWith ""
--                         --             $ Array.replicate rowSize "."
--                         --         ) <> "\n"
--                         --
--                         --     board_ =
--                         --         StringUtils.removeAll emptyLine board
--                         --
--                         --     -- _ = Debug.trace (mod (String.length board_) rowSize) (\_ -> "")
--                         -- in
--                             -- map
--                             --     (\line -> )
--                             --     (StringUtils.lines board_)
--                             board_
--                     else
--                         go updatedBoard updatedIndex updatedPoints

-- --
-- newtype Board = Board (Map Coords String)
-- -- --
-- instance showBoard :: Show Board
--     where show (Board board) = String.joinWith "" $ Array.fromFoldable $ Map.values board
-- -- -- --
-- instance boardEq :: Eq Board
--     where eq (Board lhs) (Board rhs) =
--                 let
--                     lhsKeys = Map.keys lhs
--                     rhsKeys = Map.keys rhs
--                 in
--                 lhsKeys == rhsKeys
--
-- instance boardOrd :: Ord Board
--     where compare (Board lhs) (Board rhs) =
--             let
--                 getYs =
--                     map Tuple.snd
--                         <<< Array.fromFoldable
--                         <<< Map.keys
--                 lhsKeys = getYs lhs
--                 rhsKeys = getYs rhs
--             in
--             compare lhsKeys rhsKeys

drawBoard :: Coords -> Coords -> Map Coords String
drawBoard (Coords (Tuple x y)) (Coords (Tuple x_ y_)) =
    let
        board =
            map
                (\coords ->
                    Tuple (Coords coords) "."
                )
            $ Array.concat
            $ map
                (\x ->
                    map
                        (\y ->
                            Tuple x y
                        )
                        (Array.range y y_)
                )
                (Array.range x x_)
    in
    Map.fromFoldable board

-- compareByY :: Coords -> Coords -> Ordering
-- compareByY (Coords lhs) (Coords rhs) =
--     compare (Tuple.snd lhs) (Tuple.snd rhs)
--
-- compareByX :: Coords -> Coords -> Ordering
-- compareByX (Coords lhs) (Coords rhs) =
--     compare (Tuple.fst lhs) (Tuple.fst rhs)

compareCustom :: Coords -> Coords -> Ordering
compareCustom (Coords lhs) (Coords rhs) =
    if Tuple.snd lhs == Tuple.snd rhs then
        compare (Tuple.fst lhs) (Tuple.fst rhs)
    else
        compare (Tuple.snd lhs) (Tuple.snd rhs)

getX :: Coords -> Int
getX (Coords ( Tuple x y )) = x

getY :: Coords -> Int
getY (Coords ( Tuple x y )) = y

printBoard_ :: Map Coords String -> String
printBoard_ board =
    let
        sortedKeys =
            Array.sortBy compareCustom
                $ Array.fromFoldable
                $ Map.keys board
            -- Array.fromFoldable $ Map.keys board

        _ = Debug.trace sortedKeys (\_ -> "")


        board_ =
            -- mapWithIndex
            --     (\(Coords (Tuple x y)) val ->
            --         if y == 11 then
            --             val <> "\n"
            --         else
            --             val
            --     )
            --     board
            Array.foldl
                (\res key ->
                    case Map.lookup key board of
                        Just val ->
                            if getX key == 15 then
                                res <> val <> "\n"
                            else
                                res <> val

                        Nothing ->
                            res
                )
                ""
                sortedKeys

        values =
            -- String.joinWith ""
            --     $ Array.fromFoldable
            --     $ Map.values board_
            board_
    in
    values

-- getTuple :: Coords -> Tuple Int Int
-- getTuple (Coords c) = c

applyTick_ :: Array Point -> Map Coords String -> Tuple (Array Point) (Map Coords String)
applyTick_ points board =
    Array.foldl
        (\(Tuple ps_ res) (Point { position, velocity }) ->
            let
                boardKey =
                    position

                newKey =
                    case position, velocity of
                        Coords p, Coords v ->
                            Coords
                                $ Tuple (Tuple.fst p + Tuple.fst v) (Tuple.snd p + Tuple.snd v)

                        _, _ ->
                            Coords (Tuple 0 0)
            in
            Tuple
                (Array.snoc ps_ $ Point { position : newKey, velocity : velocity })
                (Map.update (\v -> Just "#") newKey $ Map.update (\v -> Just ".") boardKey res)
        )
        (Tuple [] board)
        points


printResponse :: Array Point -> String
printResponse sortedPoints =
    let
        positions = map (\(Point p) -> p.position) sortedPoints

        xs = map (\(Coords c) -> Tuple.fst c) positions
        ys = map (\(Coords c) -> Tuple.snd c) positions

        minX = Array.foldl min 0 xs
        maxX = Array.foldl max 0 xs
        minY = Array.foldl min 0 ys
        maxY = Array.foldl max 0 ys

        initialBoard =
            drawBoard (Coords (Tuple minX minY)) (Coords (Tuple maxX maxY))

        _ = Debug.trace (printBoard_ initialBoard) (\_ -> "")

        updatedBoard =
            Array.foldl
                (\res (Point point) ->
                    Map.update (\v -> Just "#") point.position res
                )
                initialBoard
                -- (Map.fromFoldable (Tuple (Coords (Tuple 0 0)) ""))
                sortedPoints

        updatedBoard2 =
            -- applyTick_ sortedPoints
                -- $ applyTick_ sortedPoints

                -- (\(Tuple points board) -> applyTick_ points board)
                -- $ (\(Tuple points board) -> applyTick_ points board)
                -- $ (\(Tuple points board) -> applyTick_ points board)
                -- $ applyTick_ sortedPoints
                -- $ applyTick_ sortedPoints
                applyTick_ sortedPoints updatedBoard
                -- updatedBoard

        -- _ = Debug.trace (printBoard_ updatedBoard) (\_ -> "")

        boardString =
            printBoard_ (Tuple.snd updatedBoard2)

        -- _ = Debug.trace boardString (\_ -> "")

        -- _ = Debug.trace (Map.showTree updatedBoard) (\_ -> "")
    in
    -- show $ Array.sortBy (\(Point p) -> compare p.position) points
    -- show $ Array.sort points
    -- show $ points
    -- String.joinWith "" $
    -- String.joinWith "" $ Array.replicate rowSize (row <> "\n")
    -- printBoard sortedPoints minPosition maxPosition
    boardString

drawPositions :: Array Point -> String
drawPositions points_ = go 0 (Array.sort points_)
    where go tick points =
            let
                -- minPosition =
                --     getMinCoords points
                --
                -- maxPosition =
                --     getMaxCoords points

                areCloseEnough =
                    -- let
                    --     xDiff =
                    --         Tuple.fst maxPosition - Tuple.fst minPosition
                    --
                    --     yDiff =
                    --         Tuple.snd maxPosition - Tuple.snd minPosition
                    -- in
                    -- -- xDiff < 100 || yDiff < 100
                    -- xDiff < 22 && yDiff < 22
                    false


                updatedTick =
                    tick + 1

                updatedPoints =
                    if areCloseEnough then
                        points
                    else
                        -- applyTick points
                        points

                -- _ = Debug.trace (minPosition) (\_ -> "")
                -- _ = Debug.trace (maxPosition) (\_ -> "")

                -- _ = Debug.trace minPosition (\_ -> "")
                -- _ = Debug.trace maxPosition (\_ -> "")
            in
            -- if areCloseEnough then
            --     printResponse points minPosition maxPosition
            -- else
            --     go updatedTick updatedPoints
            printResponse points
            -- ""


firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day10e.txt")
    Console.log
        $ show
        -- $ (\_ -> show "")
        -- $ show
        -- $ drawPositions
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
