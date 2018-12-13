module Challenges.Day13 where

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

-- Straight paths | -
-- Curves / \
-- Intersections +
-- carts < > ^ v

-- Intersection
-- First Time  -- Left
-- Second Time -- Straight
-- Third Time  -- Right
-- Fourth Time -- GO TO FIRST TIME.

-- Same speed
-- Single cart moving at a time.

-- Current location: Based on Row.

-- A tick consists of one iteration of movement for all the carts.

-- Find the location of first crash.

-- Biggest Line on the input is 150.

type Coords = Tuple Int Int

data RoadType
    = Horizontal
    | Vertical
    | Curve String
    | Intersection

instance showRoadType :: Show RoadType
    where show Horizontal   = "Horizontal"
          show Vertical     = "Vertical"
          show (Curve d)    = "Curve " <> d
          show Intersection = "Intersection"


data Move
    = Left
    | Straight
    | Right

instance showMove :: Show Move
    where show Left     = "Left"
          show Straight = "Straight"
          show Right    = "Right"


data Direction
    = North
    | West
    | South
    | East

instance showDirecion :: Show Direction
    where show North = "North"
          show West  = "West"
          show South = "South"
          show East  = "East"

type CartStatus =
    { facingDirection :: Direction
    , nextIntersectionDirection :: Move
    , previousState :: Cell
    }

data Cell
    = Road RoadType
    | Cart CartStatus
    | Initial String
    | Empty
    | Crash

instance showCell :: Show Cell
    where show (Road type_)  = "Road " <> show type_
          show (Cart status) = "Cart " <> show status
          show (Empty)       = "Empty"
          show (Crash)       = "Crash"
          show (Initial str) = str


data Puzzle = First | Second

getBiggestLineLength :: Array String -> Int
getBiggestLineLength =
    Array.foldl
        (\res line ->
            max (String.length line) res
        )
        0


getInitialGrid :: Int -> Int -> Map Coords Cell
getInitialGrid maxX maxY =
    let
        xs = Array.range 1 maxX
        ys = Array.range 1 maxY
    in
    Map.fromFoldable
        $ map (\coords -> Tuple coords (Initial ""))
        $ Array.concat
        $ map (\x -> map (Tuple x) xs) ys


getNextIntersectionDirection :: Move -> Move
getNextIntersectionDirection Left     = Straight
getNextIntersectionDirection Straight = Right
getNextIntersectionDirection Right    = Left


getCellValue :: String -> (Cell -> Maybe Cell)
getCellValue "<"  = (\v -> Just (Cart { facingDirection : West,  nextIntersectionDirection : Left, previousState : Road Horizontal }))
getCellValue ">"  = (\v -> Just (Cart { facingDirection : East,  nextIntersectionDirection : Left, previousState : Road Horizontal }))
getCellValue "^"  = (\v -> Just (Cart { facingDirection : North, nextIntersectionDirection : Left, previousState : Road Vertical }))
getCellValue "v"  = (\v -> Just (Cart { facingDirection : South, nextIntersectionDirection : Left, previousState : Road Vertical }))
getCellValue "-"  = (\v -> Just (Road Horizontal))
getCellValue "|"  = (\v -> Just (Road Vertical))
getCellValue "\\" = (\v -> Just (Road (Curve "\\")))
getCellValue "/"  = (\v -> Just (Road (Curve "/")))
getCellValue "+"  = (\v -> Just (Road Intersection))
getCellValue _    = (\v -> Just Empty)


printCart :: CartStatus -> String
printCart { facingDirection } =
    case facingDirection of
        West  -> "<"
        East  -> ">"
        South -> "v"
        North -> "^"


getCellString :: Cell -> String
getCellString (Cart cartStatus)   = printCart cartStatus
getCellString (Road Horizontal)   = "-"
getCellString (Road Vertical)     = "|"
getCellString (Road (Curve c))    = c
getCellString (Road Intersection) = "+"
getCellString (Initial str)       = "G"
getCellString Empty               = " "
getCellString Crash               = "X"


getNextCellKey :: Coords -> Direction -> Coords
getNextCellKey (Tuple x y) direction =
    case direction of
        West  -> Tuple x       (y - 1)
        East  -> Tuple x       (y + 1)
        North -> Tuple (x - 1) y
        South -> Tuple (x + 1) y


getDirectionAfterCurve :: Direction -> String -> Direction
getDirectionAfterCurve direction c =
    case direction of
        North ->
            if c == "\\" then
                West
            else
                East
        South ->
           if c == "\\" then
               East
           else
               West
        West ->
            if c == "\\" then
                North
            else
                South
        East ->
            if c == "\\" then
                South
            else
                North

getDirectionAfterIntersection :: Direction -> Move -> Direction
getDirectionAfterIntersection direction move =
    case direction, move of
        North, Straight -> North
        North, Left     -> West
        North, Right    -> East
        South, Straight -> South
        South, Left     -> East
        South, Right    -> West
        West,  Straight -> West
        West,  Left     -> South
        West,  Right    -> North
        East,  Straight -> East
        East,  Left     -> North
        East,  Right    -> South


updateCellValue :: Puzzle -> Direction -> Move -> Maybe Cell -> Cell
updateCellValue puzzle facingDirection nextIntersectionDirection nextCell =
    case nextCell of
        Just (Road Horizontal) ->
            Cart
                { facingDirection : facingDirection
                , nextIntersectionDirection : nextIntersectionDirection
                , previousState : Road Horizontal
                }
        Just (Road Vertical) ->
            Cart
                { facingDirection : facingDirection
                , nextIntersectionDirection : nextIntersectionDirection
                , previousState : Road Vertical
                }
        Just (Road (Curve c)) ->
            Cart
                { facingDirection : getDirectionAfterCurve facingDirection c
                , nextIntersectionDirection : nextIntersectionDirection
                , previousState : Road (Curve c)
                }

        Just (Road Intersection) ->
            Cart
                { facingDirection : getDirectionAfterIntersection facingDirection nextIntersectionDirection
                , nextIntersectionDirection : getNextIntersectionDirection nextIntersectionDirection
                , previousState : Road Intersection
                }

        Just (Cart { previousState }) ->
            case puzzle of
                First ->
                    Crash
                Second ->
                    previousState

        Just (Initial str) ->
            Initial str


        Just Empty  ->
            Empty

        Just Crash ->
            Crash

        Nothing ->
            Empty


performGridWiseTick :: Puzzle -> Array Coords -> Map Coords Cell -> Map Coords Cell
performGridWiseTick puzzle cartKeys grid =
    Array.foldl
        (\res key ->
            let
                cell =
                    Map.lookup key res
            in
            case cell of
                Just (Cart { facingDirection, nextIntersectionDirection, previousState }) ->
                    let
                        updatedGrid = Map.update (\v -> Just previousState) key res

                        nextCellKey = getNextCellKey key facingDirection

                        nextCell = Map.lookup nextCellKey updatedGrid

                        updatedCellValue =
                            updateCellValue puzzle facingDirection nextIntersectionDirection nextCell
                    in
                    Map.update (\v -> Just updatedCellValue) nextCellKey updatedGrid

                _ ->
                    res
        )
        grid
        cartKeys


printGrid :: Map Coords Cell -> String
printGrid grid =
    Array.foldl
        (\res val ->
            let
                sanitizeStr =
                    String.replaceAll (String.Pattern "\\") (String.Replacement "]")

                updatedRes =
                    sanitizeStr
                        $ res <> getCellString val
            in
            if mod (String.length updatedRes) 14 == 0 then
                updatedRes <> "\n"
            else
                updatedRes
        )
        ("\n")
        (Array.fromFoldable $ Map.values grid)


getCartKeys :: Map Coords Cell -> Array Coords
getCartKeys grid =
    Array.fromFoldable
        $ Map.keys
        $ Map.mapMaybe
            (\val ->
                case val of
                    Cart _ -> Just val
                    _      -> Nothing
            )
            grid

detectCrash :: Puzzle -> Map Coords Cell -> Maybe Coords
detectCrash puzzle = go 0
    where go index grid =
            let
                crashes =
                    Array.fromFoldable
                        $ Map.keys
                        $ Map.mapMaybe
                            (\val ->
                                case val of
                                    Crash  -> Just val
                                    _      -> Nothing
                            )
                            grid
            in
            if Array.null crashes && index < 1000 then
                let
                    cartKeys =
                        getCartKeys grid

                    updatedGrid =
                        performGridWiseTick puzzle cartKeys grid
                in
                go (index + 1) updatedGrid
            else
                Array.head crashes


sanitizeInput :: Array String -> Map Coords Cell
sanitizeInput lines =
    let
        initialGrid =
            getInitialGrid (getBiggestLineLength lines) (Array.length lines)

        coordsWithValues =
            Array.zip
                (Array.fromFoldable $ Map.keys initialGrid)
                (String.split (String.Pattern "") $ String.joinWith "" lines)

        inputGrid =
            Array.foldl
                (\res (Tuple coords val) ->
                    Map.update (getCellValue val) coords res
                )
                initialGrid
                coordsWithValues
    in
    inputGrid


printAnswer :: Maybe Coords -> String
printAnswer coords =
    case coords of
        Just ( Tuple x y ) ->
            show $ Tuple (y - 1) (x - 1)

        Nothing ->
            show "Nope"

firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day13.txt")
    Console.log
        $ printAnswer
        $ detectCrash First
        $ sanitizeInput
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


findLastManStanding :: Puzzle -> Map Coords Cell -> Maybe Coords
findLastManStanding puzzle = go 0
    where go index grid =
            let
                cartKeys =
                    getCartKeys grid
            in
            if Array.length cartKeys > 1 && index < 20000 then
                let
                    updatedGrid =
                        performGridWiseTick puzzle cartKeys grid
                in
                go (index + 1) updatedGrid
            else
                Array.head cartKeys


-- Not 81,17
-- Not 80,17
-- 54,66
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day13.txt")
    Console.log
        $ show
        $ printAnswer
        $ findLastManStanding Second
        $ sanitizeInput
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
