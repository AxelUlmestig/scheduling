module Main where
import Test.Framework

import qualified RecurringPatternTest
import qualified IsActiveTest

tests = RecurringPatternTest.testCases ++
        IsActiveTest.testCases

main = defaultMain tests
