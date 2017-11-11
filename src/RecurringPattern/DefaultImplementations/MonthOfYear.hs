
module RecurringPattern.DefaultImplementations.MonthOfYear (
    MonthOfYear(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data MonthOfYear = MonthOfYear Integer

instance RecurringPattern MonthOfYear where
    unitSize    = const Month
    startTime   = nthMonthStartTime
    endTime     = nthMonthEndTime

nthMonthStartTime :: UTCTime -> MonthOfYear -> Maybe UTCTime
nthMonthStartTime currentTime (MonthOfYear n) =
    if monthsUntilNext == 0
    then
        Just currentTime
    else
        Just nextOccurrence
    where   currentDate             = utctDay currentTime
            (_, currentMonth, _)    = toGregorian currentDate
            monthsUntilNext         = (n - (toInteger currentMonth)) `mod` 12
            nextOccurrence          =
                truncateMonth
                . dateToUTC
                $ addGregorianMonthsClip monthsUntilNext currentDate

nthMonthEndTime _ (MonthOfYear 1)            = Nothing
nthMonthEndTime currentTime (MonthOfYear _)  =
    Just
    . truncateMonth
    . dateToUTC
    $ addGregorianMonthsClip 1 (utctDay currentTime)
