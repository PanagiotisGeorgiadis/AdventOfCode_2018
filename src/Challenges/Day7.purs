module Challenges.Day7 where


import Prelude

import Data.Array as Array
import Data.EuclideanRing (mod)
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
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

data Alphabet
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | NoLetter


instance showAlphabet :: Show Alphabet
    where show A = "A"
          show B = "B"
          show C = "C"
          show D = "D"
          show E = "E"
          show F = "F"
          show G = "G"
          show H = "H"
          show I = "I"
          show J = "J"
          show K = "K"
          show L = "L"
          show M = "M"
          show N = "N"
          show O = "O"
          show P = "P"
          show Q = "Q"
          show R = "R"
          show S = "S"
          show T = "T"
          show U = "U"
          show V = "V"
          show W = "W"
          show X = "X"
          show Y = "Y"
          show Z = "Z"
          show NoLetter = "Invalid letter"

instance alphabetEq :: Eq Alphabet
    where eq NoLetter rhs      = false
          eq lhs      NoLetter = false
          eq lhs      rhs      = (show lhs) == (show rhs)

instance alphabetOrd :: Ord Alphabet
    where compare NoLetter rhs      = compare ("") (show rhs)
          compare lhs      NoLetter = compare (show lhs) ("")
          compare lhs      rhs      = compare (show lhs) (show rhs)

stringToAlphabet :: String -> Alphabet
stringToAlphabet letter =
    case letter of
        "A" -> A
        "B" -> B
        "C" -> C
        "D" -> D
        "E" -> E
        "F" -> F
        "G" -> G
        "H" -> H
        "I" -> I
        "J" -> J
        "K" -> K
        "L" -> L
        "M" -> M
        "N" -> N
        "O" -> O
        "P" -> P
        "Q" -> Q
        "R" -> R
        "S" -> S
        "T" -> T
        "U" -> U
        "V" -> V
        "W" -> W
        "X" -> X
        "Y" -> Y
        "Z" -> Z
        _   -> NoLetter


newtype AlphabetObject = AlphabetObject
    { letter :: Alphabet
    , prerequisites :: Array Alphabet
    }

-- derive instance newtypeAlphabetObject :: Newtype AlphabetObject _
--
-- notComplete :: AlphabetObject -> AlphabetObject
-- notComplete (AlphabetObject r) = r { isComplete = not r.isComplete }
-- notComplete = over AlphabetObject (_ { isComplete = false })
--
-- wrap "foo" :: ClassName

instance alphabetObjectShow :: Show AlphabetObject
    where show (AlphabetObject obj) = show obj


instance alphabetObjectEq :: Eq AlphabetObject
    where eq (AlphabetObject a) (AlphabetObject b) = (show a.letter) == (show b.letter)

instance alphabetObjectOrd :: Ord AlphabetObject
    where compare (AlphabetObject a) (AlphabetObject b) = compare (show a.letter) (show b.letter)


initialAlphabet :: Array AlphabetObject
initialAlphabet =
    map (AlphabetObject)
        [ { letter : A
          , prerequisites : []
          }
        , { letter : B
          , prerequisites : []
          }
        , { letter : C
          , prerequisites : []
          }
        , { letter : D
          , prerequisites : []
          }
        , { letter : E
          , prerequisites : []
          }
        , { letter : F
          , prerequisites : []
          }
        , { letter : G
          , prerequisites : []
          }
        , { letter : H
          , prerequisites : []
          }
        , { letter : I
          , prerequisites : []
          }
        , { letter : J
          , prerequisites : []
          }
        , { letter : K
          , prerequisites : []
          }
        , { letter : L
          , prerequisites : []
          }
        , { letter : M
          , prerequisites : []
          }
        , { letter : N
          , prerequisites : []
          }
        , { letter : O
          , prerequisites : []
          }
        , { letter : P
          , prerequisites : []
          }
        , { letter : Q
          , prerequisites : []
          }
        , { letter : R
          , prerequisites : []
          }
        , { letter : S
          , prerequisites : []
          }
        , { letter : T
          , prerequisites : []
          }
        , { letter : U
          , prerequisites : []
          }
        , { letter : V
          , prerequisites : []
          }
        , { letter : W
          , prerequisites : []
          }
        , { letter : X
          , prerequisites : []
          }
        , { letter : Y
          , prerequisites : []
          }
        , { letter : Z
          , prerequisites : []
          }
        ]
-- newtype FabricInch = FabricInch { index :: Int, overlaps :: Int }
--

--
-- instance eq :: Eq FabricInch
--     where eq (FabricInch a) (FabricInch b) = a.index == b.index
--
-- instance compare :: Ord FabricInch
--     where compare (FabricInch a) (FabricInch b) = compare a.index b.index

parseInstructions :: Array String -> Array AlphabetObject
parseInstructions =
    Array.foldl
        (\res line ->
            let
                lineArray =
                    String.split (String.Pattern " ") line

                letter =
                    stringToAlphabet
                        $ Maybe.fromMaybe ""
                        $ Array.head
                        $ Array.drop 7 lineArray

                prerequisite =
                    stringToAlphabet
                        $ Maybe.fromMaybe ""
                        $ Array.head
                        $ Array.drop 1 lineArray
            in
            map
                (\(AlphabetObject obj) ->
                    if letter == obj.letter then
                        AlphabetObject (obj { prerequisites = Array.snoc obj.prerequisites prerequisite })
                    else
                        AlphabetObject obj
                )
                res
        )
        (initialAlphabet)


sortPrerequisites :: Array AlphabetObject -> Array AlphabetObject
sortPrerequisites =
    map
        (\(AlphabetObject obj) ->
            AlphabetObject (obj { prerequisites = Array.sort obj.prerequisites })
        )


removeLetterFromPrereqs :: Alphabet -> Array AlphabetObject -> Array AlphabetObject
removeLetterFromPrereqs letter =
    map
        (\(AlphabetObject obj) ->
            AlphabetObject (obj { prerequisites = Array.filter ((/=) letter) obj.prerequisites })
        )


getFreedObjects :: Array AlphabetObject -> Array AlphabetObject
getFreedObjects =
    Array.filter
        (\(AlphabetObject obj) ->
            Array.null obj.prerequisites
        )

solve :: Array AlphabetObject -> String
solve alphabetObjects = go alphabetObjects ""
    where go objects res =
            let
                freedObjects =
                    getFreedObjects objects

                freedLetters =
                    map (\(AlphabetObject obj) -> obj.letter) freedObjects

                updatedObjPrerequisites =
                    Array.foldl
                        (\res letter ->
                            removeLetterFromPrereqs letter res
                        )
                        objects
                        freedLetters

                updatedObjects =
                    Array.filter
                        (\(AlphabetObject obj) ->
                            Array.foldl
                                (\res item ->
                                    if res then
                                        res
                                    else
                                        not $ obj.letter == item
                                )
                                false
                                freedLetters
                        )
                        updatedObjPrerequisites

                lettersToAdd =
                    map show $ Array.sort freedLetters

                updatedResult =
                    Array.foldl
                        (\r letter ->
                            if String.contains (String.Pattern letter) r then
                                r
                            else
                                r <> letter
                        )
                        res
                        lettersToAdd

                -- updatedObjects =
                --     Array.filter
                --         (\(AlphabetObject obj) ->
                --             not $ Array.null obj.prerequisites
                --         )
                --         updatedObjPrerequisites

                _ = Debug.trace updatedObjects (\_ -> "")
                -- _ = Debug.trace updatedObjPrerequisites (\_ -> "")
                -- updatedObjectsAndResult =
                --     Array.foldl
                --         (\res letter ->
                --             let
                --                 -- _ = Debug.trace letter (\_ -> "")
                --                 updatedObjs =
                --                     -- Array.filter (\(AlphabetObject obj) -> not $ Array.null obj.prerequisites )
                --                     removeLetterFromPrereqs letter (Tuple.fst res)
                --
                --                 -- _ = Debug.trace updatedObjs (\_ -> "")
                --
                --                 lettersToAdd =
                --                     map show $ Array.sort freedLetters
                --
                --                 updatedRes =
                --                     Array.foldl
                --                         (\r letter ->
                --                             if String.contains (String.Pattern letter) r then
                --                                 r
                --                             else
                --                                 r <> letter
                --                         )
                --                         (Tuple.snd res)
                --                         lettersToAdd
                --             in
                --             Tuple updatedObjs updatedRes
                --         )
                --         (Tuple objects res)
                --         freedLetters
                --
                -- -- _ = Debug.trace (Tuple.fst updatedObjectsAndResult) (\_ -> "")
                --
                -- updatedResult =
                --     Tuple.snd updatedObjectsAndResult
                --
                -- updatedObjects =
                --     Array.filter (\(AlphabetObject obj) -> not $ Array.null obj.prerequisites)
                --         $ Tuple.fst updatedObjectsAndResult
                --
                --
                -- _ = Debug.trace updatedResult (\_ -> "")
                -- _ = Debug.trace updatedObjects (\_ -> "")
                -- lettersToAdd =
                --     map show $ Array.sort freedLetters

                -- updatedRes =
                --     Array.foldl
                --         (\r letter ->
                --             if String.contains (String.Pattern letter) r then
                --                 r
                --             else
                --                 r <> letter
                --         )
                --         res
                --         lettersToAdd

                -- areAllLettersFreed =
                --     Array.all identity
                --         $ map Array.null
                --         $ map (\(AlphabetObject obj) -> obj.prerequisites) objects
            in
            -- if Array.null objects then
            --     res
            -- else
            --     go updatedObjects updatedResult
            ""


-- JKXDEPTFABUHOQSVYZMLNCIGRW -- Wrong
-- JKXDEPTFABUHOQSVYZMLNCIGRW
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day7.txt")
    Console.log
        $ show
        $ solve
        $ sortPrerequisites
        $ parseInstructions
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day7.txt")
    Console.log
        $ show
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
