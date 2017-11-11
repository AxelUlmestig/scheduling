
module RecurringPattern.DefaultImplementations.DayOfMonth (
    DayOfMonth(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data DayOfMonth = DayOfMonth Integer

instance RecurringPattern DayOfMonth where
    unitSize        = const Day
    startTime       = dayOfMonthStartTime
    endTime         = dayOfMonthEndTime

dayOfMonthStartTime :: UTCTime -> DayOfMonth -> Maybe UTCTime
dayOfMonthStartTime _ (DayOfMonth 1)                    = Nothing
dayOfMonthStartTime currentTime (DayOfMonth targetDay)  =
    if daysUntilNext == 0
    then
        Just currentTime
    else
        nextOccurrence
    where   nextOccurrence                          = dayOfMonthStartTime possibleNextOccurrence (DayOfMonth targetDay)
            possibleNextOccurrence                  = dateToUTC $ addDays (toInteger daysUntilNext) currentDate
            daysUntilNext                           = (targetDay - (toInteger currentDay)) `mod` toInteger daysInMonth
            (currentYear, currentMonth, currentDay) = toGregorian $ utctDay currentTime
            currentDate                             = utctDay currentTime
            daysInMonth                             = gregorianMonthLength currentYear currentMonth

dayOfMonthEndTime _ (DayOfMonth 1)            = Nothing
dayOfMonthEndTime currentTime (DayOfMonth _)  = Just . dateToUTC $ addDays 1 (utctDay currentTime)
