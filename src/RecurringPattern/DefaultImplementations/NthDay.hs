
module RecurringPattern.DefaultImplementations.NthDay (
    NthDay(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthDay = NthDay Integer

instance RecurringPattern NthDay where
    unitSize        = const Year
    startTime       = nthDayStartTime
    endTime         = nthDayEndTime

nthDayStartTime :: UTCTime -> UTCTime -> NthDay -> UTCTime
nthDayStartTime _ _ (NthDay 1)                            = endOfTime
nthDayStartTime scheduleStartTime currentTime (NthDay n)  =
    if daysRemainder == 0
    then
        currentTime
    else
        nextOccurrence
    where   currentDate     = utctDay currentTime
            daysSinceStart  = diffDays currentDate (utctDay scheduleStartTime)
            daysRemainder   = daysSinceStart `mod` n
            daysUntilNext   = toInteger $ n - daysRemainder
            nextOccurrence  = dateToUTC $ addDays daysUntilNext currentDate

nthDayEndTime _ (NthDay 1)            = endOfTime
nthDayEndTime currentTime (NthDay _)  = dateToUTC $ addDays 1 (utctDay currentTime)
