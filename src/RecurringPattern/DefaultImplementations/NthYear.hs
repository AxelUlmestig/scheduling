
module RecurringPattern.DefaultImplementations.NthYear (
    NthYear(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthYear = NthYear Integer

instance RecurringPattern NthYear where
    unitSize        = const Year
    startTime       = nthYearStartTime
    endTime         = nthYearEndTime
    isInfiniteLoop  = nthYearIsInfiniteLoop

nthYearStartTime :: UTCTime -> UTCTime -> NthYear -> UTCTime
nthYearStartTime _ _ (NthYear 1)                            = endOfTime
nthYearStartTime scheduleStartTime currentTime (NthYear n)  =
    if yearsUntilNext == 0
    then
        currentTime
    else
        nextOccurrence
    where   (currentYear, _, _) = toGregorian (utctDay currentTime)
            (startYear, _, _)   = toGregorian (utctDay scheduleStartTime)
            yearsUntilNext      = (startYear - currentYear) `mod` n
            monthsUntilNext     = 12 * yearsUntilNext
            currentDate         = utctDay currentTime
            nextOccurrence      = truncateYear . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

nthYearEndTime _ (NthYear 1)            = endOfTime
nthYearEndTime currentTime (NthYear n)  = truncateYear . dateToUTC $ addGregorianYearsClip 1 (utctDay currentTime)

nthYearIsInfiniteLoop :: UTCTime -> NthYear -> UTCTime -> Bool
nthYearIsInfiniteLoop startTime _ currentTime = realToFrac (diffUTCTime startTime currentTime) > 10 * 365 * 24 * 60 ^ 2
