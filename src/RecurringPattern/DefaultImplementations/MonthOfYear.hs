
module RecurringPattern.DefaultImplementations.MonthOfYear (
    MonthOfYear(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data MonthOfYear = MonthOfYear Integer

instance RecurringPattern MonthOfYear where
    unitSize        = const Year
    startTime       = nthMonthStartTime
    endTime         = nthMonthEndTime
    isInfiniteLoop  = nthMonthIsInfiniteLoop

nthMonthStartTime :: UTCTime -> UTCTime -> MonthOfYear -> UTCTime
nthMonthStartTime _ currentTime (MonthOfYear n) =
    if monthsUntilNext == 0
    then
        currentTime
    else
        nextOccurrence
    where   currentDate             = utctDay currentTime
            (_, currentMonth, _)    = toGregorian currentDate
            monthsUntilNext         = (n - (toInteger currentMonth)) `mod` 12
            nextOccurrence          = truncateMonth . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

nthMonthEndTime _ (MonthOfYear 1)            = endOfTime
nthMonthEndTime currentTime (MonthOfYear _)  = truncateMonth . dateToUTC $ addGregorianMonthsClip 1 (utctDay currentTime)

nthMonthIsInfiniteLoop :: UTCTime -> MonthOfYear -> UTCTime -> Bool
nthMonthIsInfiniteLoop startTime _ currentTime = realToFrac (diffUTCTime startTime currentTime) > 10 * 365 * 24 * 60 ^ 2
