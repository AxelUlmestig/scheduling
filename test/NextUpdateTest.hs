{-# LANGUAGE OverloadedStrings #-}

module NextUpdateTest (
    testCases
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time

import RecurringPattern
import NextUpdate

inactive1 = assertEqual "12/3 every two years, one year too early" expected actual
    where   expected            = UTCTime (fromGregorian 2019 3 12) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2018 4 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

inactive2 = assertEqual "12/3 every two years, earlier same month" expected actual
    where   expected            = UTCTime (fromGregorian 2017 3 12) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 3) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

inactive3 = assertEqual "12/3 every two years, later same month" expected actual
    where   expected            = UTCTime (fromGregorian 2019 3 12) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 14) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

inactive4 = assertEqual "the fourth and fifth of every month, 2/2" expected actual
    where   expected            = UTCTime (fromGregorian 2017 2 4) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 2 2) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [DayOfMonth 4, DayOfMonth 5]

inactive5 = assertEqual "the fourth and fifth of every month, 17/4" expected actual
    where   expected            = UTCTime (fromGregorian 2017 5 4) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 4 17) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [DayOfMonth 4, DayOfMonth 5]

active1 = assertEqual "12/3 every two years" expected actual
    where   expected            = UTCTime (fromGregorian 2017 3 13) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, MonthOfYear 3, DayOfMonth 12]

active2 = assertEqual "the fourth and fifth of every month, 4/3" expected actual
    where   expected            = UTCTime (fromGregorian 2017 3 6) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 4) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [DayOfMonth 4, DayOfMonth 5]

active3 = assertEqual "the fourth and fifth of every month, 5/3" expected actual
    where   expected            = UTCTime (fromGregorian 2017 3 6) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 5) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [DayOfMonth 4, DayOfMonth 5]

active4 = assertEqual "the 12th every month every two years" expected actual
    where   expected            = UTCTime (fromGregorian 2017 3 13) 0
            actual              = nextUpdate scheduleStartTime currentTime recurringPatterns
            currentTime         = UTCTime (fromGregorian 2017 3 12) (3 * 60 ^ 2 + 23 * 60)
            scheduleStartTime   = UTCTime (fromGregorian 2017 1 1) (5 * 60 ^ 2 + 11 * 60)
            recurringPatterns   = [NthYear 2, NthMonth 1, DayOfMonth 12]

assertions = [
        inactive1,
        inactive2,
        inactive3,
        inactive4,
        inactive5,
        active1,
        active2,
        active3,
        active4
    ]

labels = ["next update " ++ show n | n <- [1..]]

testCases = hUnitTestToTests . TestList . zipWith TestLabel labels . map TestCase $ assertions
