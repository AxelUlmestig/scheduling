
module RecurringPattern.DefaultImplementations.NthYear (
    NthYear(..)
) where

import Data.Time

import RecurringPattern
import TimeUtil

data NthYear = NthYear Integer UTCTime

instance RecurringPattern NthYear where
    unitSize        = const Year
    startTime       = nthYearStartTime
    endTime         = nthYearEndTime

nthYearStartTime :: UTCTime -> NthYear -> Maybe UTCTime
nthYearStartTime currentTime (NthYear 1 _)              = Just currentTime
nthYearStartTime currentTime (NthYear n referencePoint) =
    if yearsUntilNext == 0
    then
        Just currentTime
    else
        Just nextOccurrence
    where   (currentYear, _, _) = toGregorian (utctDay currentTime)
            (startYear, _, _)   = toGregorian (utctDay referencePoint)
            yearsUntilNext      = (startYear - currentYear) `mod` n
            monthsUntilNext     = 12 * yearsUntilNext
            currentDate         = utctDay currentTime
            nextOccurrence      = truncateYear . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

nthYearEndTime _ (NthYear 1 _)              = Nothing
nthYearEndTime currentTime (NthYear n _)    =
    Just
    . truncateYear
    . dateToUTC
    $ addGregorianYearsClip 1 (utctDay currentTime)
