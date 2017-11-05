
module RecurringPattern.DefaultImplementations.NthMonth (
    NthMonth(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthMonth = NthMonth Integer

instance RecurringPattern NthMonth where
    unitSize        = const Year
    startTime       = nthMonthStartTime
    endTime         = nthMonthEndTime
    isInfiniteLoop  = nthMonthIsInfiniteLoop

nthMonthStartTime :: UTCTime -> UTCTime -> NthMonth -> UTCTime
nthMonthStartTime _ _ (NthMonth 1)                            = endOfTime
nthMonthStartTime scheduleStartTime currentTime (NthMonth n)  =
    if monthsUntilNext == 0
    then
        currentTime
    else
        nextOccurrence
    where   currentDate                             = utctDay currentTime
            (currentYear, currentMonthInYear, _)    = toGregorian currentDate
            (startYear, startMonthInYear, _)        = toGregorian (utctDay scheduleStartTime)
            currentMonth                            = currentYear * 12 + (toInteger currentMonthInYear)
            startMonth                              = startYear * 12 + (toInteger startMonthInYear)
            monthsUntilNext                         = (startMonth - currentMonth) `mod` n
            nextOccurrence                          = truncateMonth . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

nthMonthEndTime _ (NthMonth 1)            = endOfTime
nthMonthEndTime currentTime (NthMonth _)  = truncateMonth . dateToUTC $ addGregorianMonthsClip 1 (utctDay currentTime)

nthMonthIsInfiniteLoop :: UTCTime -> NthMonth -> UTCTime -> Bool
nthMonthIsInfiniteLoop startTime _ currentTime = realToFrac (diffUTCTime startTime currentTime) > 10 * 365 * 24 * 60 ^ 2
