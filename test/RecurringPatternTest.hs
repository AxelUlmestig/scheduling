{-# LANGUAGE OverloadedStrings #-}

module RecurringPatternTest (
    testCases
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time

import RecurringPattern.RecurringPattern
import TimeUtil

import RecurringPattern.DefaultImplementations

startTimeBeforeScheduleStart = assertEqual "start time before schedule start" expected actual
    where   expected            = Just scheduleStartTime
            actual              = nextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2015 3 19) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6 scheduleStartTime

startTimeEveryThirdYear1 = assertEqual "every third year, start year" expected actual
    where   expected            = Just currentTime
            actual              = nextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2017 11 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthYear 3 scheduleStartTime

startTimeEveryThirdYear2 = assertEqual "every third year, year 4" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2023 1 1) 0
            actual              = nextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2021 11 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthYear 3 scheduleStartTime

startTimeEveryFifthMonth1 = assertEqual "every fifth month, start month" expected actual
    where   expected            = Just currentTime
            actual              = nextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2017 10 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthMonth 5 scheduleStartTime

startTimeEveryFifthMonth2 = assertEqual "every fifth month, month 11" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2019 1 1) 0
            actual              = nextStartTime scheduleStartTime currentTime everyThirdYear
            currentTime         = UTCTime (fromGregorian 2018 9 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 14) (11 * 60 ^ 2 + 42 * 60)
            everyThirdYear      = NthMonth 5 scheduleStartTime

startTimeEveryMay1 = assertEqual "every May, same month" expected actual
    where   expected            = Just currentTime
            actual              = nextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 5 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

startTimeEveryMay2 = assertEqual "every May, July" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2019 5 1) 0
            actual              = nextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 7 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

startTimeEveryMay3 = assertEqual "every May, Mars" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2018 5 1) 0
            actual              = nextStartTime scheduleStartTime currentTime everyMay
            currentTime         = UTCTime (fromGregorian 2018 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = currentTime
            everyMay            = MonthOfYear 5

startTimeEverySixDays1 = assertEqual "every six days, start day" expected actual
    where   expected            = Just currentTime
            actual              = nextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2017 10 12) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6 scheduleStartTime

startTimeEverySixDays2 = assertEqual "every six days, day 7" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 10 24) 0
            actual              = nextStartTime scheduleStartTime currentTime everySixDays
            currentTime         = UTCTime (fromGregorian 2017 10 19) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 12) (3 * 60 ^ 2 + 23 * 60)
            everySixDays        = NthDay 6 scheduleStartTime

startTimeTheThirtiethDayOfEveryMonth1 = assertEqual "the 30th of every month, day 30" expected actual
    where   expected                    = Just currentTime
            actual                      = nextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 30) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 30) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

startTimeTheThirtiethDayOfEveryMonth2 = assertEqual "the 30th of every month, 31/1" expected actual
    where   expected                    = Just $ UTCTime (fromGregorian 2017 3 30) 0
            actual                      = nextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 31) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 30) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

startTimeTheThirtiethDayOfEveryMonth3 = assertEqual "the 30th of every month, 14/1" expected actual
    where   expected                    = Just $ UTCTime (fromGregorian 2017 1 30) 0
            actual                      = nextStartTime scheduleStartTime currentTime theThirtiethOfEveryMonth
            currentTime                 = UTCTime (fromGregorian 2017 1 14) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime           = UTCTime (fromGregorian 2017 1 1) (3 * 60 ^ 2 + 23 * 60)
            theThirtiethOfEveryMonth    = DayOfMonth 30

endTimeEveryYear = assertEqual "every year" expected actual
    where   expected            = Nothing
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2017 6 2) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 14) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthYear 1 scheduleStartTime

endTimeEveryTwoYears1 = assertEqual "every two years, start year" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2018 1 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2017 6 2) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 14) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthYear 2 scheduleStartTime

endTimeEveryTwoYears2 = assertEqual "every two years, one year passed" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2020 1 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2018 6 2) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 14) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthYear 2 scheduleStartTime

endTimeEveryMonth = assertEqual "every month" expected actual
    where   expected            = Nothing
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2017 6 2) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 14) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthMonth 1 scheduleStartTime

endTimeEveryThreeMonths1 = assertEqual "every three months, start month" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 4 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2017 3 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 4) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthMonth 3 scheduleStartTime

endTimeEveryThreeMonths2 = assertEqual "every three months, one month passed" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 7 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyTwoYears
            currentTime         = UTCTime (fromGregorian 2017 4 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 4) (13 * 60 ^ 2 + 34 * 60)
            everyTwoYears       = NthMonth 3 scheduleStartTime

endTimeEveryJune1 = assertEqual "every june, june" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 7 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyJune
            currentTime         = UTCTime (fromGregorian 2017 6 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 6 12) (5 * 60 ^ 2 + 54 * 60)
            everyJune           = MonthOfYear 6

endTimeEveryJune2 = assertEqual "every june, october" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2018 7 1) 0
            actual              = nextEndTime scheduleStartTime currentTime everyJune
            currentTime         = UTCTime (fromGregorian 2017 10 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 6 12) (5 * 60 ^ 2 + 54 * 60)
            everyJune           = MonthOfYear 6

endTimeEveryDay = assertEqual "every day" expected actual
    where   expected            = Nothing
            actual              = nextEndTime scheduleStartTime currentTime everyDay
            currentTime         = UTCTime (fromGregorian 2017 10 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 18) (5 * 60 ^ 2 + 54 * 60)
            everyDay            = NthDay 1 scheduleStartTime

endTimeEveryFiveDays1 = assertEqual "every five days, start day" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 10 19) 0
            actual              = nextEndTime scheduleStartTime currentTime everyFiveDays
            currentTime         = UTCTime (fromGregorian 2017 10 18) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 18) (5 * 60 ^ 2 + 54 * 60)
            everyFiveDays       = NthDay 5 scheduleStartTime

endTimeEveryFiveDays2 = assertEqual "every five days, 3 days passed" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 10 24) 0
            actual              = nextEndTime scheduleStartTime currentTime everyFiveDays
            currentTime         = UTCTime (fromGregorian 2017 10 21) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 10 18) (14 * 60 ^ 2 + 48 * 60)
            everyFiveDays       = NthDay 5 scheduleStartTime

endTimeFourthDayOfEveryMonth1 = assertEqual "the fourth every month, 4/3" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 3 5) 0
            actual              = nextEndTime scheduleStartTime currentTime theFourth
            currentTime         = UTCTime (fromGregorian 2017 3 4) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 1) (14 * 60 ^ 2 + 48 * 60)
            theFourth           = DayOfMonth 4

endTimeFourthDayOfEveryMonth2 = assertEqual "the fourth every month, 5/3" expected actual
    where   expected            = Just $ UTCTime (fromGregorian 2017 4 5) 0
            actual              = nextEndTime scheduleStartTime currentTime theFourth
            currentTime         = UTCTime (fromGregorian 2017 3 5) (5 * 60 ^ 2 + 54 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 3 1) (14 * 60 ^ 2 + 48 * 60)
            theFourth           = DayOfMonth 4

assertions = [
        startTimeBeforeScheduleStart,
        startTimeEveryThirdYear1,
        startTimeEveryThirdYear2,
        startTimeEveryFifthMonth1,
        startTimeEveryFifthMonth2,
        startTimeEveryMay1,
        startTimeEveryMay2,
        startTimeEveryMay3,
        startTimeEverySixDays1,
        startTimeEverySixDays2,
        startTimeTheThirtiethDayOfEveryMonth1,
        startTimeTheThirtiethDayOfEveryMonth2,
        startTimeTheThirtiethDayOfEveryMonth3,
        endTimeEveryYear,
        endTimeEveryTwoYears1,
        endTimeEveryTwoYears2,
        endTimeEveryMonth,
        endTimeEveryThreeMonths1,
        endTimeEveryThreeMonths2,
        endTimeEveryJune1,
        endTimeEveryJune2,
        endTimeEveryDay,
        endTimeEveryFiveDays1,
        endTimeEveryFiveDays2,
        endTimeFourthDayOfEveryMonth1,
        endTimeFourthDayOfEveryMonth2
    ]

labels = ["startTime " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels . map TestCase $ assertions
