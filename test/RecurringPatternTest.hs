{-# LANGUAGE OverloadedStrings #-}

module RecurringPatternTest (
    testCases
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time

import RecurringPattern

startTimeBeforeScheduleStart = assertEqual "start time before schedule start" expected actual
    where   expected            = scheduleStartTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2015 3 19) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6

startTimeEveryThirdYear1 = assertEqual "every third year, start year" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2017 11 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthYear 3

startTimeEveryThirdYear2 = assertEqual "every third year, year 4" expected actual
    where   expected            = UTCTime (fromGregorian 2023 1 1) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2021 11 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthYear 3

startTimeEveryFifthMonth1 = assertEqual "every fifth month, start month" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2017 10 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthMonth 5

startTimeEveryFifthMonth2 = assertEqual "every fifth month, month 11" expected actual
    where   expected            = UTCTime (fromGregorian 2019 1 1) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2018 9 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthMonth 5

startTimeEveryMay1 = assertEqual "every May, same month" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 5 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

startTimeEveryMay2 = assertEqual "every May, July" expected actual
    where   expected            = UTCTime (fromGregorian 2019 5 1) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 7 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

startTimeEveryMay3 = assertEqual "every May, Mars" expected actual
    where   expected            = UTCTime (fromGregorian 2018 5 1) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

{-
startTimeEveryFourthWeek1 = assertEqual "every fourth week, start week" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyFourthWeek
            currentTime         = UTCTime (fromGregorian 2017 10 14) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everyFourthWeek     = NthWeek 4

startTimeEveryFourthWeek2 = assertEqual "every fourth week, week 5" expected actual
    where   expected            = UTCTime (fromGregorian 2017 12 6) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyFourthWeek
            currentTime         = UTCTime (fromGregorian 2017 11 15) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everyFourthWeek     = NthWeek 4

startTimeEveryFourthWeek3 = assertEqual "every fourth week, week 8" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everyFourthWeek
            currentTime         = UTCTime (fromGregorian 2017 12 6) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everyFourthWeek     = NthWeek 4
-}

startTimeEverySixDays1 = assertEqual "every six days, start day" expected actual
    where   expected            = currentTime
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2017 10 12) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6

startTimeEverySixDays2 = assertEqual "every six days, day 7" expected actual
    where   expected            = UTCTime (fromGregorian 2017 10 24) 0
            actual              = recurringPatternNextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2017 10 19) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6

startTimeTheThirtiethDayOfEveryMonth1 = assertEqual "the 30th of every month, day 30" expected actual
    where   expected                    = currentTime
            actual                      = recurringPatternNextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 30) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 30) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

startTimeTheThirtiethDayOfEveryMonth2 = assertEqual "the 30th of every month, 31/1" expected actual
    where   expected                    = UTCTime (fromGregorian 2017 3 30) 0
            actual                      = recurringPatternNextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 31) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 30) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

startTimeTheThirtiethDayOfEveryMonth3 = assertEqual "the 30th of every month, 14/1" expected actual
    where   expected                    = UTCTime (fromGregorian 2017 1 30) 0
            actual                      = recurringPatternNextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 14) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 1) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

assertions = [
        startTimeBeforeScheduleStart,
        startTimeEveryThirdYear1,
        startTimeEveryThirdYear2,
        startTimeEveryFifthMonth1,
        startTimeEveryFifthMonth2,
        startTimeEveryMay1,
        startTimeEveryMay2,
        startTimeEveryMay3,
        {-
        startTimeEveryFourthWeek1,
        startTimeEveryFourthWeek2,
        startTimeEveryFourthWeek3
        -}
        startTimeEverySixDays1,
        startTimeEverySixDays2,
        startTimeTheThirtiethDayOfEveryMonth1,
        startTimeTheThirtiethDayOfEveryMonth2,
        startTimeTheThirtiethDayOfEveryMonth3
    ]

labels = ["startTime " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels . map TestCase $ assertions
