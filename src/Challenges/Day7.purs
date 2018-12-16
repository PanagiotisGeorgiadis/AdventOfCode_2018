module Challenges.Day7 where


import Prelude

import Data.Array as Array
import Data.EuclideanRing (mod)
import Data.Foldable (sum)
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
import Data.Tuple as Tuple
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
    , isComplete :: Boolean
    , duration :: Int
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
          , isComplete : false
          , duration : 1
          }
        , { letter : B
          , prerequisites : []
          , isComplete : false
          , duration : 2
          }
        , { letter : C
          , prerequisites : []
          , isComplete : false
          , duration : 3
          }
        , { letter : D
          , prerequisites : []
          , isComplete : false
          , duration : 4
          }
        , { letter : E
          , prerequisites : []
          , isComplete : false
          , duration : 5
          }
        , { letter : F
          , prerequisites : []
          , isComplete : false
          , duration : 6
          }
        , { letter : G
          , prerequisites : []
          , isComplete : false
          , duration : 7
          }
        , { letter : H
          , prerequisites : []
          , isComplete : false
          , duration : 8
          }
        , { letter : I
          , prerequisites : []
          , isComplete : false
          , duration : 9
          }
        , { letter : J
          , prerequisites : []
          , isComplete : false
          , duration : 10
          }
        , { letter : K
          , prerequisites : []
          , isComplete : false
          , duration : 11
          }
        , { letter : L
          , prerequisites : []
          , isComplete : false
          , duration : 12
          }
        , { letter : M
          , prerequisites : []
          , isComplete : false
          , duration : 13
          }
        , { letter : N
          , prerequisites : []
          , isComplete : false
          , duration : 14
          }
        , { letter : O
          , prerequisites : []
          , isComplete : false
          , duration : 15
          }
        , { letter : P
          , prerequisites : []
          , isComplete : false
          , duration : 16
          }
        , { letter : Q
          , prerequisites : []
          , isComplete : false
          , duration : 17
          }
        , { letter : R
          , prerequisites : []
          , isComplete : false
          , duration : 18
          }
        , { letter : S
          , prerequisites : []
          , isComplete : false
          , duration : 19
          }
        , { letter : T
          , prerequisites : []
          , isComplete : false
          , duration : 20
          }
        , { letter : U
          , prerequisites : []
          , isComplete : false
          , duration : 21
          }
        , { letter : V
          , prerequisites : []
          , isComplete : false
          , duration : 22
          }
        , { letter : W
          , prerequisites : []
          , isComplete : false
          , duration : 23
          }
        , { letter : X
          , prerequisites : []
          , isComplete : false
          , duration : 24
          }
        , { letter : Y
          , prerequisites : []
          , isComplete : false
          , duration : 25
          }
        , { letter : Z
          , prerequisites : []
          , isComplete : false
          , duration : 26
          }
        ]

setDuration :: Array AlphabetObject -> Array AlphabetObject
setDuration =
    map
        (\(AlphabetObject obj) ->
            AlphabetObject (obj { duration = obj.duration + 60 })
        )


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
        initialAlphabet


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
                    Array.take 1
                        $ map
                            (\(AlphabetObject obj) ->
                                obj.letter
                            )
                            freedObjects

                updatedObjects_ =
                    Array.filter
                        (\(AlphabetObject obj) ->
                            case Array.head freedLetters of
                                Just letter ->
                                    obj.letter /= letter

                                Nothing ->
                                    false
                        )
                        objects

                updatedObjPrerequisites =
                    Array.foldl
                        (\res letter ->
                            removeLetterFromPrereqs letter res
                        )
                        updatedObjects_
                        freedLetters

                lettersToAdd =
                    map show
                        $ Array.sort freedLetters

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
            in
            if Array.null objects then
                res
            else
                go updatedObjPrerequisites updatedResult


-- JDEKPFABTUHOQSXVYMLZCNIGRW
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


decrementDuration :: AlphabetObject -> AlphabetObject
decrementDuration (AlphabetObject obj) =
    AlphabetObject (obj { duration = obj.duration - 1 })

getLetter :: AlphabetObject -> Alphabet
getLetter (AlphabetObject obj) =
    obj.letter

solve_ :: Array AlphabetObject -> Int
solve_ = go 0 []
    where go duration workers objects =
            let
                freedObjects =
                    getFreedObjects objects

                activeWorkers =
                    Array.foldl
                        (\res freed ->
                            let
                                isAlreadyActive =
                                    Array.any identity
                                        $ map (\r -> getLetter r == getLetter freed) res
                            in
                            if Array.length res < 5 && not isAlreadyActive then
                                Array.snoc res freed
                            else
                                res
                        )
                        workers
                        freedObjects

                updatedWorkers_ =
                    map decrementDuration activeWorkers

                updatedDuration =
                    duration + 1

                updatedObjects =
                    Array.foldl
                        (\res (AlphabetObject worker) ->
                            if worker.duration == 0 then
                                Array.filter (\(AlphabetObject worker_) -> worker_.letter /= worker.letter)
                                    $ removeLetterFromPrereqs worker.letter res
                            else
                                res
                        )
                        objects
                        updatedWorkers_

                updatedWorkers =
                    Array.filter (\(AlphabetObject worker) -> worker.duration > 0) updatedWorkers_
            in
            if Array.null objects then
                duration
            else
                go updatedDuration updatedWorkers updatedObjects


-- 1048 ???
secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day7.txt")
    Console.log
        $ show
        $ solve_
        $ setDuration
        $ sortPrerequisites
        $ parseInstructions
        $ map (StringUtils.removeAll "\r")
        $ getInputLines contents
