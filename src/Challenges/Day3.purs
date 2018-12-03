module Challenges.Day3 where

import Prelude
import Data.Foldable (foldl)


import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Async (writeTextFile)
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.Maybe as Maybe
import Utils.String as StringUtils
--import Utils.Array as Array
--import Utils.Helpers (getInputLines)

type FabricClaimPartial =
    { leftOffset :: Int
    , topOffset :: Int
    , width :: Int
    , height :: Int
    -- , minY :: Int
    -- , maxY :: Int
    -- , minX :: Int
    -- , maxX :: Int
    }

-- type Dimension =
--     Tuple Int Int

type FabricClaim =
    { id :: Int
    , leftOffset :: Int
    , topOffset :: Int
    , width :: Int
    , height :: Int
    , dimensions :: Array (Array Int)
    }


parseId :: String -> Int
parseId str =
    Maybe.withDefault 0 $ Int.fromString $ String.drop 1 str


parseLeftOffset :: String -> Int
parseLeftOffset str =
    Maybe.withDefault 0
        $ Int.fromString
        $ Maybe.withDefault ""
        $ Array.head
        $ String.split (String.Pattern ",") str


parseTopOffset :: String -> Int
parseTopOffset str =
    Maybe.withDefault 0
        $ Int.fromString
        $ StringUtils.removeAll ":"
        $ Maybe.withDefault ""
        $ Array.head
        $ Array.drop 1
        $ String.split (String.Pattern ",") str


parseWidth :: String -> Int
parseWidth str =
    Maybe.withDefault 0
        $ Int.fromString
        $ Maybe.withDefault ""
        $ Array.head
        $ String.split (String.Pattern "x") str


parseHeight :: String -> Int
parseHeight str =
    Maybe.withDefault 0
        $ Int.fromString
        $ Maybe.withDefault ""
        $ Array.head
        $ Array.reverse
        $ String.split (String.Pattern "x") str


getCoords :: FabricClaimPartial -> Array (Array Int)
getCoords { leftOffset, topOffset, width, height } =
    let
        minY =
            topOffset

        maxY =
            topOffset + height

        minX =
            leftOffset

        maxX =
            leftOffset + width
    in
    Array.concat
        $ map (\x -> map (\y -> [x, y]) (Array.range minY maxY)) (Array.range minX maxX)
-- getFabricDimensions :: FabricClaimPartial -> Array (Array Int)
-- getFabricDimensions { leftOffset, topOffset, width, height } =
--     let
--         one =
--             Array.range leftOffset (leftOffset + width)
--
--         two =
--             Array.range topOffset (topOffset + height)
--     in
--     Array.concat
--         $ map (\x -> map (\y -> [x,y]) two) one

parseFabricClaim :: String -> FabricClaim
parseFabricClaim claim = go (String.split (String.Pattern " ") claim)
    where go [ id, _, offset, dimensions ] =
            let
                leftOffset =
                    parseLeftOffset offset

                topOffset =
                    parseTopOffset offset

                width =
                    parseWidth dimensions

                height =
                    parseHeight dimensions

                minX =
                    leftOffset

                maxX =
                    leftOffset + width

                minY =
                    topOffset

                maxY =
                    topOffset + height

                partialClaim =
                    { leftOffset : leftOffset
                    , topOffset : topOffset
                    , width : width
                    , height : height
                    -- , minX : minX
                    -- , maxX : maxX
                    -- , minY : minY
                    -- , maxY : maxY
                    }
            in
            { id : parseId id
            , leftOffset : leftOffset
            , topOffset : topOffset
            , width : width
            , height : height
            , dimensions : getCoords partialClaim
            -- , minY : topOffset
            -- , maxY : topOffset + height
            -- , minX : leftOffset
            -- , maxX : leftOffset + width
            }
          go _ =
            { id : 0
            , leftOffset : 0
            , topOffset : 0
            , width : 0
            , height : 0
            , dimensions : []
            -- , minY : 0
            -- , maxY : 0
            -- , minX : 0
            -- , maxX : 0
            }


removeCoord :: Array Int -> Array (Array Int) -> Array (Array Int)
removeCoord coord =
    Array.filter ((/=) coord)


countDuplicateCoords :: Array (Array Int) -> Int
countDuplicateCoords = go 0
    where go count coords =
            case Array.head coords of
                Just c ->
                    let
                        updatedCount =
                            if Array.any ((==) c) (Array.drop 1 coords) then
                                count + 1
                            else
                                count
                    in
                    go updatedCount (removeCoord c coords)

                Nothing ->
                    count


-- getDuplicateCoords :: Array (Array Int) -> Array (Array Int)
-- getDuplicateCoords = go []
--     where go res array =
--             case Array.head array of
--                 Just elem ->
--                     let
--                         updatedArray =
--                             Array.drop 1 array
--                     in
--                     if Array.any ((==) elem) updatedArray then
--                         go (res <> [elem]) (removeCoord elem updatedArray)
--                     else
--                         go res updatedArray
--
--                 Nothing ->
--                     res


isDuplicate :: Array Int -> Array (Array Int) -> Boolean
isDuplicate dimension array =
    Array.elemIndex dimension array /= Array.elemLastIndex dimension array


-- 11899
getDuplicateCoords2 :: Array (Array Int) -> Array (Array Int)
getDuplicateCoords2 array = go (Array.nub array) array []
    where go coordSet coordArray res =
            case Array.head coordSet, Array.head coordArray of
                Just s, Just a ->
                    if s == a then
                        go (Array.drop 1 coordSet) (Array.drop 1 coordArray) res
                    else
                        go coordSet (Array.drop 1 coordArray) (Array.snoc res a)

                _, _ ->
                    Array.nub res

            -- if Array.take 1 coordSet == Array.take 1 coordArray then
            --     go (Array.drop 1 coordSet) (Array.drop 1 coordArray) (res <> Array.take 1 coordSet)
            -- else
            --     go coordSet (Array.drop 1 coordArray) res
            --
            -- Array.foldl
            --     (\res item ->
            --         -- if coordArray
            --         res <> [ item ]
            --         -- Array.snoc res item
            --     )
            --     []
            --     coordSet
            --
    -- Array.foldl
    --     (\res item ->
    --         -- if isDuplicate item array && Array.elemIndex item res == Nothing then
    --         --     Array.snoc res item
    --         -- else
    --         --     res
    --     )
    --     []
    --     array

            -- if Array.elemIndex item res == Nothing then
            -- if Array.nubEq
                -- Array.snoc res item
            -- else
                -- res
-- countOverlappingInches :: FabricClaim -> FabricClaim -> Int
-- countOverlappingInches claim claim_ = go (getCoords claim) (getCoords claim_) 0
--     where go coords coords_ count =
--             case Array.head coords of
--                 Just h ->
--                     let
--                         updatedCount =
--                             if Array.any ((==) h) coords then
--                                 count + 1
--                             else
--                                 count
--                     in
--                     go (Array.drop 1 coords) coords_ updatedCount
--
--                 Nothing ->
--                     count

-- getDuplicateCoords :: Set (Array Int) -> Set (Array Int)
-- getDuplicateCoords set =
--     foldl
--         (\res item ->
--             if Set.member item $ Set.delete item set then
--                 Set.insert item res
--             else
--                 res
--         )
--         (Set.fromFoldable [])
--         set


firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    -- _ <- (writeTextFile UTF8 "./src/PuzzleInputs/Day3a.txt") $ Array.concat $ map _.dimensions $ map parseFabricClaim $ map (StringUtils.removeAll "\r") $ getInputLines contents
    Console.log
        $ show
        -- $ countDuplicateCoords
        -- $ Array.length
        -- $ Array.fromFoldable
        -- $ Set.size
        -- $ getDuplicateCoords
        $ getDuplicateCoords2
        $ Set.fromFoldable
        $ Array.sort
        -- $ Array.take 200000
        $ Array.take 150000
        -- $ getDuplicateCoords
        $ Array.concat
        -- $ map _.dimensions (_ + 1) (case _ of ...) (if _ then a else b)
        $ map _.dimensions
        $ map parseFabricClaim
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    Console.log
        $ show
        $ getInputLines contents
