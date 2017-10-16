{-# LANGUAGE OverloadedStrings #-}

module IsActiveTest (
    testCases
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time

import RecurringPattern.RecurringPattern
import RecurringPattern.IsActive

isActive1 = assertBool "the 12/3 every second year, 12/3 start year" currentlyActive
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

isActive2 = assertBool "the 12/3 every second year, 12/3 year 4" currentlyActive
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2021 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

isInactiveActive1 = assertBool "the 12/3 every second year, 13/3 start year" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 13) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

isInactiveActive2 = assertBool "the 12/3 every second year, 12/4 start year" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 4 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

isInactiveActive3 = assertBool "the 12/3 every second year, 12/3 year 1" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2018 4 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

assertions = [
        isActive1,
        isActive2,
        isInactiveActive1,
        isInactiveActive2,
        isInactiveActive3
    ]

labels = ["is active " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels . map TestCase $ assertions
