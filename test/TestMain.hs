module Main where
import Test.Framework

import qualified RecurringPatternTest
import qualified IsActiveTest
import qualified NextUpdateTest

tests = RecurringPatternTest.testCases ++
        IsActiveTest.testCases ++
        NextUpdateTest.testCases

main = defaultMain tests
