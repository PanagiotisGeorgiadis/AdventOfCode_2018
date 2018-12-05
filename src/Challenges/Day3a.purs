module Challenges.Day3a where

import Prelude

import Control.Monad.Gen (resize)
import Data.Array as Array
import Data.Int as Int
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


-- newtype Fabric = Fabric (Array { x :: Int, y :: Int, overlaps :: Int })
newtype Fabric = Fabric (Array { index :: Int, overlaps :: Int })

instance showFabric :: Show Fabric
    where show (Fabric fabrics) = String.joinWith "\n" $ map show fabrics

newtype FabricInch = FabricInch { index :: Int, overlaps :: Int }

instance showFabricInch :: Show FabricInch
    where show (FabricInch fabricInch) = show fabricInch

instance eq :: Eq FabricInch
    where eq (FabricInch a) (FabricInch b) = a.index == b.index

instance compare :: Ord FabricInch
    where compare (FabricInch a) (FabricInch b) = compare a.index b.index

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
    , coords :: Array (Tuple Int Int)
    -- , dimensions :: Array (Array Int)
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
            topOffset + height

        minX =
            leftOffset

        maxX =
            leftOffset + width
    in
    -- Array.concat
        -- $ map (\x -> map (\y -> [x, y]) (Array.range minY maxY)) (Array.range minX maxX)
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
coordsToIndex (Tuple x y) = x * y


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

                coords =
                    getCoords partialClaim
            in
            { id : parseId id
            , leftOffset : leftOffset
            , topOffset : topOffset
            , width : width
            , height : height
            , coords : coords
            -- , dimensions : coords
            , indexes : map coordsToIndex coords
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
            , coords : []
            -- , dimensions : []
            , indexes : []
            -- , minY : 0
            -- , maxY : 0
            -- , minX : 0
            -- , maxX : 0
            }

fabricTake :: Int -> Fabric -> Fabric
fabricTake count (Fabric fabric) =
    Fabric (Array.take count fabric)

initialFabric :: Fabric
initialFabric =
    -- Fabric
    --     $ Array.concat
    --     $ map
    --         (\x ->
    --             map
    --                 (\y ->
    --                     { x : x
    --                     , y : y
    --                     , overlaps : 0
    --                     }
    --                 )
    --                 (Array.range 0 999)
    --         )
    --         (Array.range 0 999)
    Fabric
        $ Array.concat
        $ map
            (\x ->
                map
                    (\y ->
                        { index : x * y
                        , overlaps : 0
                        }
                    )
                    (Array.range 1 1000)
            )
            (Array.range 1 1000)

addClaimOnFabric :: Int -> Fabric -> Fabric
addClaimOnFabric index (Fabric fabric) =
    Fabric
        $ map
            (\inch ->
                if inch.index == index then
                    inch { overlaps = inch.overlaps + 1 }
                else
                    inch
            )
            fabric

-- applyFabricClaims :: Array FabricClaim -> Fabric
-- applyFabricClaims =
--     Array.foldl
--         (\fabric claim ->
--             Array.foldl
--                 (\res index ->
--                     addClaimOnFabric index res
--                 )
--                 fabric
--                 claim.indexes
--         )
--         initialFabric

applyFabricClaims :: Array Int -> Fabric
applyFabricClaims =
    Array.foldl
        (\fabric index ->
            -- Array.foldl
                -- (\res index ->
                    addClaimOnFabric index fabric
                -- )
                -- fabric
                -- claim.indexes
        )
        initialFabric

initialFabricInch :: Set FabricInch
initialFabricInch =
    Set.singleton (FabricInch { index : 0, overlaps : 0 })

incrementOverlap :: Int -> Set FabricInch -> Set FabricInch
incrementOverlap index =
    Set.map
        (\(FabricInch fabricInch) ->
            if fabricInch.index == index then
                FabricInch (fabricInch { overlaps = fabricInch.overlaps + 1 })
            else
                (FabricInch fabricInch)
        )

-- fabric indexes == 543780
-- fabric indexes == 543780
solve :: Array Int -> Set FabricInch
solve =
    Array.foldl
        (\res index ->
            let
                tempInch =
                    (FabricInch { index : index, overlaps : 0 })
            in
            if Set.member tempInch res then
                incrementOverlap index res
            else
                Set.insert tempInch res
        )
        initialFabricInch



-- countOverlappingInches :: Fabric -> Int
-- countOverlappingInches (Fabric fabric) =
--     Array.foldl
--         (\res inch ->
--             if inch.overlaps > 1 then
--                 res + 1
--             else
--                 res
--         )
--         0
--         fabric


countOverlappingInches :: Set FabricInch -> Int
countOverlappingInches inches =
    Array.foldl
        (\res (FabricInch inch) ->
            if inch.overlaps > 1 then
                res + 1
            else
                res
        )
        0
        (Array.fromFoldable inches)

-- fabric claims  == 1323
-- fabric indexes == 543780
-- Fabric indexes == 1000000
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    Console.log
        $ show
        -- $ Array.length
        -- $ Array.concat
        -- $ map _.indexes
        $ countOverlappingInches
        -- $ applyFabricClaims
        $ solve
        $ Array.concat
        $ map _.indexes
        $ map parseFabricClaim
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day3.txt")
    Console.log
        $ show
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
