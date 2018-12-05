module Challenges.Day3a where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.Maybe as Maybe
import Utils.String as StringUtils


newtype Fabric = Fabric (Array { index :: Int, overlaps :: Int })

instance showFabric :: Show Fabric
    where show (Fabric fabrics) = String.joinWith "\n" $ map show fabrics

-- newtype FabricInch = FabricInch { index :: Int, overlaps :: Int }
--
-- instance showFabricInch :: Show FabricInch
--     where show (FabricInch fabricInch) = show fabricInch
--
-- instance eq :: Eq FabricInch
--     where eq (FabricInch a) (FabricInch b) = a.index == b.index
--
-- instance compare :: Ord FabricInch
--     where compare (FabricInch a) (FabricInch b) = compare a.index b.index

type FabricClaimPartial =
    { leftOffset :: Int
    , topOffset :: Int
    , width :: Int
    , height :: Int
    }

type FabricClaim =
    { id :: Int
    , leftOffset :: Int
    , topOffset :: Int
    , width :: Int
    , height :: Int
    , coords :: Array (Tuple Int Int)
    , indexes :: Array Int
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


getCoords :: FabricClaimPartial -> Array (Tuple Int Int)
getCoords { leftOffset, topOffset, width, height } =
    let
        minY =
            topOffset

        maxY =
            topOffset + height - 1

        minX =
            leftOffset

        maxX =
            leftOffset + width - 1
    in
    Array.concat
        $ map
            (\x ->
                map
                    (\y ->
                        Tuple x y
                    )
                    (Array.range minY maxY)
            )
            (Array.range minX maxX)


coordsToIndex :: Tuple Int Int -> Int
coordsToIndex (Tuple x y) = (x * 1000) + y + 1


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
                    }

                coords =
                    getCoords partialClaim
            in
            { id : parseId id
            , leftOffset : leftOffset
            , topOffset : topOffset
            , width : width
            , height : height
            , coords : coords
            , indexes : map coordsToIndex coords
            }
          go _ =
            { id : 0
            , leftOffset : 0
            , topOffset : 0
            , width : 0
            , height : 0
            , coords : []
            , indexes : []
            }


fabricMap :: Map Int Int
fabricMap =
    Map.fromFoldable
        $ map (\index -> Tuple index 0) (Array.range 1 1000000)


countOverlappingInches :: Map Int Int -> Int
countOverlappingInches inches =
    Array.length
        $ Array.filter (\item -> item > 1)
        $ (Array.fromFoldable inches)


drawClaimsOnFabric :: Array FabricClaim -> Map Int Int
drawClaimsOnFabric claims =
    let
        indexes =
            Array.concat
                $ map _.indexes claims
    in
    Array.foldl
        (\res index ->
            Map.update (\v -> Just (v + 1)) index res
        )
        fabricMap
        indexes

-- 175642 -- Too high
-- 8755 -- Not Correct >_<
-- 109785 -- YES
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    Console.log
        $ show
        $ countOverlappingInches
        $ drawClaimsOnFabric
        $ map parseFabricClaim
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


findNonOverlappingClaim :: Array FabricClaim -> Map Int Int -> Int
findNonOverlappingClaim claims fabricMap =
    Array.foldl
        (\res claim ->
            let
                indexValues =
                    map (\index -> Map.lookup index fabricMap) claim.indexes
            in
            if Array.all ((==) (Just 1)) indexValues then
                claim.id
            else
                res
        )
        0
        claims


-- 504 -- YES
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    Console.log
        $ show
        $ (\claims -> findNonOverlappingClaim claims (drawClaimsOnFabric claims))
        $ map parseFabricClaim
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
