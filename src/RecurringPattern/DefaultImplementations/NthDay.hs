
module RecurringPattern.DefaultImplementations.NthDay (
    NthDay(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthDay = NthDay Integer UTCTime

instance RecurringPattern NthDay where
    unitSize        = const Day
    startTime       = nthDayStartTime
    endTime         = nthDayEndTime

nthDayStartTime :: UTCTime -> NthDay -> Maybe UTCTime
nthDayStartTime currentTime (NthDay 1 _)                = Just currentTime
nthDayStartTime currentTime (NthDay n referencePoint)   =
    if daysRemainder == 0
    then
        Just currentTime
    else
        Just nextOccurrence
    where   currentDate     = utctDay currentTime
            daysSinceStart  = diffDays currentDate (utctDay referencePoint)
            daysRemainder   = daysSinceStart `mod` n
            daysUntilNext   = toInteger $ n - daysRemainder
            nextOccurrence  = dateToUTC $ addDays daysUntilNext currentDate

nthDayEndTime _ (NthDay 1 _)            = Nothing
nthDayEndTime currentTime (NthDay _ _)  = Just . dateToUTC $ addDays 1 (utctDay currentTime)
