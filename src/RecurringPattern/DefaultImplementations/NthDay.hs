
module RecurringPattern.DefaultImplementations.NthDay (
    NthDay(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthDay = NthDay Integer UTCTime

instance RecurringPattern NthDay where
    unitSize        = const Year
    startTime       = nthDayStartTime
    endTime         = nthDayEndTime

nthDayStartTime :: UTCTime -> NthDay -> UTCTime
nthDayStartTime _ (NthDay 1 _)             = endOfTime
nthDayStartTime currentTime (NthDay n referencePoint)  =
    if daysRemainder == 0
    then
        currentTime
    else
        nextOccurrence
    where   currentDate     = utctDay currentTime
            daysSinceStart  = diffDays currentDate (utctDay referencePoint)
            daysRemainder   = daysSinceStart `mod` n
            daysUntilNext   = toInteger $ n - daysRemainder
            nextOccurrence  = dateToUTC $ addDays daysUntilNext currentDate

nthDayEndTime _ (NthDay 1 _)            = endOfTime
nthDayEndTime currentTime (NthDay _ _)  = dateToUTC $ addDays 1 (utctDay currentTime)
