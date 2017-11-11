
module RecurringPattern.DefaultImplementations.NthMonth (
    NthMonth(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthMonth = NthMonth Integer UTCTime

instance RecurringPattern NthMonth where
    unitSize        = const Month
    startTime       = nthMonthStartTime
    endTime         = nthMonthEndTime

nthMonthStartTime :: UTCTime -> NthMonth -> Maybe UTCTime
nthMonthStartTime currentTime (NthMonth 1 _)                = Just currentTime
nthMonthStartTime currentTime (NthMonth n referencePoint)   =
    if monthsUntilNext == 0
    then
        Just currentTime
    else
        Just nextOccurrence
    where   currentDate                             = utctDay currentTime
            (currentYear, currentMonthInYear, _)    = toGregorian currentDate
            (startYear, startMonthInYear, _)        = toGregorian (utctDay referencePoint)
            currentMonth                            = currentYear * 12 + (toInteger currentMonthInYear)
            startMonth                              = startYear * 12 + (toInteger startMonthInYear)
            monthsUntilNext                         = (startMonth - currentMonth) `mod` n
            nextOccurrence                          = truncateMonth . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

nthMonthEndTime _ (NthMonth 1 _)            = Nothing
nthMonthEndTime currentTime (NthMonth _ _)  =
    Just
    . truncateMonth
    . dateToUTC
    $ addGregorianMonthsClip 1 (utctDay currentTime)
