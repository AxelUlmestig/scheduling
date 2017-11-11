{-# LANGUAGE OverloadedStrings #-}

module IsActiveTest (
    testCases
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time

import RecurringPattern.RecurringPattern
import RecurringPattern.IsActive
import RecurringPattern.DefaultImplementations

isActive1 = assertBool "the 12/3 every second year, 12/3 start year" currentlyActive
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper (NthYear 2 scheduleStartTime), RPWrapper(MonthOfYear 3), RPWrapper(DayOfMonth 12)]

isActive2 = assertBool "the 12/3 every second year, 12/3 year 4" currentlyActive
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2021 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthYear 2 scheduleStartTime), RPWrapper(MonthOfYear 3), RPWrapper(DayOfMonth 12)]

isActive3 = assertBool "every month" currentlyActive
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthMonth 1 scheduleStartTime)]

isInactive1 = assertBool "every other year, one year has passed" (not currentlyActive)
    where   currentlyActive     = isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2018 3 13) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthYear 2 scheduleStartTime)]

isInactive2 = assertBool "the 12:th every month, the 13:th" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 13) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(DayOfMonth 12)]

isInactive3 = assertBool "the 12/3 every second year, 13/3 start year" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 13) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthYear 2 scheduleStartTime), RPWrapper(MonthOfYear 3), RPWrapper(DayOfMonth 12)]

isInactive4 = assertBool "the 12/3 every second year, 12/4 start year" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 4 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthYear 2 scheduleStartTime), RPWrapper(MonthOfYear 3), RPWrapper(DayOfMonth 12)]

isInactive5 = assertBool "the 12/3 every second year, 12/3 year 1" currentlyInactive
    where   currentlyInactive   = not $ isActive scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2018 4 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [RPWrapper(NthYear 2 scheduleStartTime), RPWrapper(MonthOfYear 3), RPWrapper(DayOfMonth 12)]

assertions = [
        isActive1,
        isActive2,
        isActive3,
        isInactive1,
        isInactive2,
        isInactive3,
        isInactive4,
        isInactive5
    ]

labels = ["is active " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels . map TestCase $ assertions
