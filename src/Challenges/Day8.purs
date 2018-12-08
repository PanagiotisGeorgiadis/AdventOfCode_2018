module Challenges.Day8 where


import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
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


newtype Node = Node
    { metadata :: Array Int
    , children :: Array Node
    }

instance nodeShow :: Show Node
    where show (Node obj) = show obj


type ParseNodeRes =
    { numbers :: Array Int
    , node :: Node
    }

type ParseChildRes =
    { numbers :: Array Int
    , nodes :: Array Node
    }


parseNode :: Array Int -> ParseNodeRes
parseNode numbers =
    let
        childrenCount =
            Maybe.fromMaybe 0
                $ Array.head numbers

        metadataCount =
            Maybe.fromMaybe 0
                $ Array.head
                $ Array.drop 1 numbers

        parseLeafRes =
            getChildren childrenCount
                { numbers : Array.drop 2 numbers
                , nodes : []
                }

        metadata =
            (Array.take metadataCount parseLeafRes.numbers)
    in
    { numbers : Array.drop metadataCount parseLeafRes.numbers
    , node :
        Node
            { metadata : metadata
            , children : parseLeafRes.nodes
            }
    }


getChildren :: Int -> ParseChildRes -> ParseChildRes
getChildren childrenCount res =
    if childrenCount == Array.length res.nodes then
        res
    else
        let
            newNode =
                parseNode res.numbers

            updatedRes =
                { numbers : newNode.numbers
                , nodes : Array.snoc res.nodes newNode.node
                }
        in
        getChildren childrenCount updatedRes


parseLicenseFile :: String -> Node
parseLicenseFile line =
    let
        numbers =
            map (Maybe.fromMaybe (-1))
                $ map Int.fromString
                $ String.split (String.Pattern " ") line

        nodeResult =
            parseNode numbers
    in
    nodeResult.node


sumMetadata :: Node -> Int
sumMetadata (Node node) =
    sum node.metadata + (sum $ map sumMetadata node.children)


-- 48155
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day8.txt")
    Console.log
        $ show
        $ sumMetadata
        $ parseLicenseFile
        $ Maybe.fromMaybe ""
        $ Array.head
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


sumValues :: Node -> Int
sumValues (Node node) =
    if Array.null node.children then
        sum node.metadata
    else
        let
            childNodes =
                Array.foldl
                    (\res index ->
                        Array.snoc res (Array.head $ Array.drop (index - 1) node.children)
                    )
                    []
                    node.metadata
        in
        Array.foldl
            (\res child ->
                case child of
                    Just c ->
                        res + sumValues c
                    Nothing ->
                        res
            )
            0
            childNodes

-- 40292
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day8.txt")
    Console.log
        $ show
        $ sumValues
        $ parseLicenseFile
        $ Maybe.fromMaybe ""
        $ Array.head
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
