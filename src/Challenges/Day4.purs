module Challenges.Day4 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
-- import Debug.Trace as Debug
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils.Helpers (getInputLines)
import Utils.Maybe as Maybe
import Utils.String as StringUtils

type Guidelines =
    { date :: Date
    , time :: Time
    , action :: GuardAction
    }

type Date =
    { year :: Int
    , month :: Int
    , day :: Int
    }

type Time =
    { hours :: Int
    , minutes :: Int
    }


type DateTime =
    { date :: Date
    , time :: Time
    }

data GuardAction
    = WakesUp
    | FallsAsleep
    | BeginsShift Int

instance show :: Show GuardAction
    where show action =
            case action of
                WakesUp ->
                    "WakesUp"
                FallsAsleep ->
                    "FallsAsleep"
                BeginsShift guardId ->
                    "BeginsShift " <> show guardId


parseDate :: String -> Date
parseDate line =
    let
        dateStr =
            StringUtils.removeAll "["
                $ Maybe.withDefault ""
                $ Array.head
                $ String.split (String.Pattern " ")
                $ Maybe.withDefault ""
                $ Array.head
                $ String.split (String.Pattern "] ") line

        dateArray =
            String.split (String.Pattern "-") dateStr

        parseInt =
            Maybe.withDefault 0 <<< Int.fromString <<< Maybe.withDefault ""
    in
    { year : parseInt $ Array.head dateArray
    , month : parseInt $ Array.head $ Array.drop 1 dateArray
    , day : parseInt $ Array.head $ Array.drop 2 dateArray
    }


parseTime :: String -> Time
parseTime line =
    let
        timeStr =
            Maybe.withDefault ""
                $ Array.head
                $ Array.drop 1
                $ String.split (String.Pattern " ")
                $ Maybe.withDefault ""
                $ Array.head
                $ String.split (String.Pattern "] ") line

        timeArray =
            String.split (String.Pattern ":") timeStr

        parseInt =
            Maybe.withDefault 0 <<< Int.fromString <<< Maybe.withDefault ""
    in
    { hours : parseInt $ Array.head timeArray
    , minutes : parseInt $ Array.head $ Array.drop 1 timeArray
    }


parseGuardAction :: String -> GuardAction
parseGuardAction line =
    let
        actionStr =
            Maybe.withDefault ""
                $ Array.head
                $ Array.drop 1
                $ String.split (String.Pattern "] ") line
    in
    case actionStr of
        "wakes up" ->
            WakesUp
        "falls asleep" ->
            FallsAsleep
        _ ->
            let
                guardId =
                    Maybe.withDefault 0
                        $ Int.fromString
                        $ Maybe.withDefault ""
                        $ Array.head
                        $ String.split (String.Pattern " ")
                        $ Maybe.withDefault ""
                        $ Array.head
                        $ Array.drop 1
                        $ String.split (String.Pattern "#" ) actionStr
            in
            BeginsShift guardId


parseGuideline :: String -> Guidelines
parseGuideline line =
    { date : parseDate line
    , time : parseTime line
    , action : parseGuardAction line
    }


compareDateTime :: Guidelines -> Guidelines -> Ordering
compareDateTime lhs rhs =
    if lhs.date.year > rhs.date.year then
        GT
    else if lhs.date.year < rhs.date.year then
        LT
    else if lhs.date.month > rhs.date.month then
        GT
    else if lhs.date.month < rhs.date.month then
        LT
    else if lhs.date.day > rhs.date.day then
        GT
    else if lhs.date.day < rhs.date.day then
        LT
    else if lhs.time.hours > rhs.time.hours then
        GT
    else if lhs.time.hours < rhs.time.hours then
        LT
    else if lhs.time.minutes > rhs.time.minutes then
        GT
    else if lhs.time.minutes < rhs.time.minutes then
        LT
    else
        EQ


initialDateTime :: DateTime
initialDateTime =
    { date :
        { year : 0
        , month : 0
        , day : 0
        }
    , time :
        { hours : 0
        , minutes : 0
        }
    }

-- getMonthLastDay :: Date -> Int
-- getMonthLastDay { month } =
--     if Array.any ((==) month) [1,3,5,7,8,10,12] then
--         31
--     else if month == 2 then
--         28
--     else
--         30
--
--
-- daysTillEndOfMonth :: Date -> Int
-- daysTillEndOfMonth date =
--     getMonthLastDay date - date.day
--
--
-- getDifferenceInDays :: Date -> Date -> Int
-- getDifferenceInDays lhs rhs =
--     if lhs.year < rhs.year then
--         -15
--     else if lhs.month < rhs.month then
--         daysTillEndOfMonth lhs + rhs.day
--     else if lhs.day > rhs.day then
--         daysTillEndOfMonth lhs + rhs.day
--     else
--         rhs.day - lhs.day


getMinutesSet :: Set (Tuple Int Int)
getMinutesSet =
    Set.fromFoldable
        $ map
            (\min ->
                Tuple min 0
            )
            (Array.concat [1..59])


newtype Guard = Guard { id :: Int, minutes :: Set (Tuple Int Int) }

instance eq :: Eq Guard
    where eq (Guard a) (Guard b) = a.id == b.id

instance compare :: Ord Guard
    where compare (Guard a) (Guard b) = compare a.id b.id

instance showGuard :: Show Guard
    where show (Guard guard) = show guard


hoursTillEndOfDay :: Time -> Int
hoursTillEndOfDay { hours } =
    if hours == 0 then
        0
    else
        24 - hours


minutesTillEndOfHour :: Time -> Int
minutesTillEndOfHour { minutes } =
    if minutes == 0 then
        0
    else
        60 - minutes


getMinuteDifference :: Time -> Time -> Int
getMinuteDifference lhs rhs =
    let
        minuteDifference =
            if lhs.minutes > rhs.minutes then
                minutesTillEndOfHour lhs + rhs.minutes
            else
                rhs.minutes - lhs.minutes
    in
    if lhs.hours > rhs.hours then
        hoursTillEndOfDay lhs + rhs.hours + minuteDifference
    else
        rhs.hours - lhs.hours + minuteDifference



addOneOnMinute :: Time -> Set (Tuple Int Int) -> Set (Tuple Int Int)
addOneOnMinute { minutes } =
    Set.map
        (\tuple ->
            if minutes == Tuple.fst tuple then
                Tuple (Tuple.fst tuple) (Tuple.snd tuple + 1)
            else
                Tuple (Tuple.fst tuple) (Tuple.snd tuple)
        )


getId :: Guard -> Int
getId (Guard guard) = guard.id


parseGuardShifts :: Array Guidelines -> Set Guard
parseGuardShifts = go (Set.empty) 0 initialDateTime
    where go guards lastId lastDateTime guidelines =
            case Array.head guidelines of
                    Just guideline ->
                        let
                            updatedGuidelines =
                                Array.drop 1 guidelines
                        in
                        case guideline.action of
                            WakesUp ->
                                let
                                    { date, time } =
                                        lastDateTime

                                    minutesDifference =
                                        getMinuteDifference time guideline.time - 1

                                    newTimes =
                                        map
                                            (\min ->
                                                let
                                                    min_ =
                                                        if min > 59 then
                                                            min - 60
                                                        else
                                                            min
                                                in
                                                { hours : 0
                                                , minutes : min_
                                                }
                                            )
                                            (Array.range time.minutes (time.minutes + minutesDifference))

                                    updatedGuards =
                                        Set.map
                                            (\guard ->
                                                if getId guard == lastId then
                                                    let
                                                        updatedGuard =
                                                            Array.foldl
                                                                (\(Guard guard_) time ->
                                                                    Guard (guard_ { minutes = addOneOnMinute time guard_.minutes })
                                                                )
                                                                guard
                                                                newTimes
                                                    in
                                                    updatedGuard

                                                else
                                                    guard
                                            )
                                            guards
                                in
                                go updatedGuards lastId lastDateTime updatedGuidelines

                            FallsAsleep ->
                                let
                                    updatedDateTime =
                                        { date : guideline.date
                                        , time : guideline.time
                                        }
                                in
                                go guards lastId updatedDateTime updatedGuidelines

                            BeginsShift guardId ->
                                let
                                    newGuard =
                                        Guard { id : guardId, minutes : getMinutesSet }

                                    updatedGuards =
                                        if Set.member newGuard guards then
                                            guards
                                        else
                                            Set.insert newGuard guards
                                in
                                go updatedGuards guardId lastDateTime updatedGuidelines
                    Nothing ->
                        guards

getTotalSleepDuration :: Guard -> Int
getTotalSleepDuration (Guard guard) =
    Array.foldl ((+)) 0
        $ map Tuple.snd
        $ Array.fromFoldable
        $ guard.minutes


getSleepiestMinute :: Guard -> Int
getSleepiestMinute (Guard guard) =
    Tuple.fst
        $ Array.foldl
            (\res minute ->
                if Tuple.snd minute > Tuple.snd res then
                    minute
                else
                    res
            )
            (Tuple 0 0)
            (Array.fromFoldable guard.minutes)


getSleepiestMinuteDuration :: Guard -> Int
getSleepiestMinuteDuration (Guard guard) =
    let
        sleepiestMinute =
            getSleepiestMinute (Guard guard)
    in
    Array.foldl
        (\res tuple ->
            if Tuple.fst tuple == sleepiestMinute then
                Tuple.snd tuple
            else
                res
        )
        0
        (Array.fromFoldable guard.minutes)


type SleepiestGuard =
    { guard :: Guard
    , sleepiestMinute :: Int
    , sleepDuration :: Int
    , sleepiestMinuteDuration :: Int
    }

findSleepiestGuard :: Array Guard -> SleepiestGuard
findSleepiestGuard =
    Array.foldl
        (\res guard ->
            let
                newTotalSleep =
                    getTotalSleepDuration guard

                oldTotalSleep =
                    getTotalSleepDuration (res.guard)
            in
            if newTotalSleep > oldTotalSleep then
                { guard : guard
                , sleepiestMinute : getSleepiestMinute guard
                , sleepDuration : newTotalSleep
                , sleepiestMinuteDuration : getSleepiestMinuteDuration guard
                }
            else
                res
        )
        ( { guard : (Guard { id : 0, minutes : getMinutesSet })
          , sleepiestMinute : 0
          , sleepDuration : 0
          , sleepiestMinuteDuration : 0
          }
        )

-- 101262
firstChallenge :: Effect Unit
firstChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day4.txt")
    Console.log
        $ show
        $ findSleepiestGuard
        $ Array.fromFoldable
        $ parseGuardShifts
        $ Array.sortBy compareDateTime
        $ map parseGuideline
        $ getInputLines contents


findSleepiestMinuteGuard :: Array Guard -> SleepiestGuard
findSleepiestMinuteGuard =
    Array.foldl
        (\res guard ->
            let
                newTotalSleep =
                    getSleepiestMinuteDuration guard

                oldTotalSleep =
                    getSleepiestMinuteDuration (res.guard)
            in
            if newTotalSleep > oldTotalSleep then
                { guard : guard
                , sleepiestMinute : getSleepiestMinute guard
                , sleepDuration : newTotalSleep
                , sleepiestMinuteDuration : getSleepiestMinuteDuration guard
                }
            else
                res
        )
        ( { guard : (Guard { id : 0, minutes : getMinutesSet })
          , sleepiestMinute : 0
          , sleepDuration : 0
          , sleepiestMinuteDuration : 0
          }
        )


secondChallenge :: Effect Unit
secondChallenge = do
    contents <- try (readTextFile UTF8 "./src/PuzzleInputs/Day4.txt")
    Console.log
        $ show
        $ findSleepiestMinuteGuard
        $ Array.fromFoldable
        $ parseGuardShifts
        $ Array.sortBy compareDateTime
        $ map parseGuideline
        $ getInputLines contents
