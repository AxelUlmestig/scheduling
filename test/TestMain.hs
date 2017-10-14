module Main where
import Test.Framework

import qualified RecurringPatternTest

tests =     RecurringPatternTest.testCases

main = defaultMain tests
