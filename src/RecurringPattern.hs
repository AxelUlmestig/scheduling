
module RecurringPattern (
    RecurringPattern(..),
    unitSize,
    recurringPatternNextStartTime
) where

import Data.Time

import TimeUtil

data UnitSize =
    Year    |
    Month   |
    Week    |
    Day     |
    Second
    deriving (Eq, Show, Ord)

data RecurringPattern =
    NthYear Int     |
    NthMonth Int    |
    MonthOfYear Int |
    NthWeek Int     |
    --WeekOfMonth Int |
    NthDay Int      |
    DayOfMonth Int  |
    DayOfWeek Int   |
    ClockTimeOfDay Int Int

unitSize :: RecurringPattern -> UnitSize
unitSize (NthYear _)            = Year
unitSize (NthMonth _)           = Month
unitSize (MonthOfYear _)        = Month
--unitSize (NthWeek _)            = Week
--unitSize (WeekOfMonth _)        = Week
unitSize (NthDay _)             = Day
unitSize (DayOfMonth _)         = Day
unitSize (DayOfWeek _)          = Day
unitSize (ClockTimeOfDay _ _)   = Second

recurringPatternNextStartTime :: UTCTime -> UTCTime -> RecurringPattern -> UTCTime
recurringPatternNextStartTime scheduleStartTime currentTime recurringPattern
    | scheduleStartTime > currentTime                                       = recurringPatternNextStartTime scheduleStartTime scheduleStartTime recurringPattern
recurringPatternNextStartTime scheduleStartTime currentTime (NthYear n)     = nthYearStartTime scheduleStartTime currentTime (toInteger n)
recurringPatternNextStartTime scheduleStartTime currentTime (NthMonth n)    = nthMonthStartTime scheduleStartTime currentTime (toInteger n)
recurringPatternNextStartTime scheduleStartTime currentTime (MonthOfYear n) = monthOfYearStartTime currentTime n
--recurringPatternNextStartTime scheduleStartTime currentTime (NthWeek n)     = nthWeekStartTime scheduleStartTime currentTime n
recurringPatternNextStartTime scheduleStartTime currentTime (DayOfMonth n)  = dayOfMonthStartTime currentTime n
recurringPatternNextStartTime scheduleStartTime currentTime (NthDay n)      = nthDayStartTime scheduleStartTime currentTime (toInteger n)

nthYearStartTime :: UTCTime -> UTCTime -> Integer -> UTCTime
nthYearStartTime scheduleStartTime currentTime n =
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

nthMonthStartTime :: UTCTime -> UTCTime -> Integer -> UTCTime
nthMonthStartTime scheduleStartTime currentTime n =
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

monthOfYearStartTime :: UTCTime -> Int -> UTCTime
monthOfYearStartTime currentTime month =
    if monthsUntilNext == 0
    then
        currentTime
    else
        nextOccurrence
    where   currentDate             = utctDay currentTime
            (_, currentMonth, _)    = toGregorian currentDate
            monthsUntilNext         = toInteger $ (month - currentMonth) `mod` 12
            nextOccurrence          = truncateMonth . dateToUTC $ addGregorianMonthsClip monthsUntilNext currentDate

{-
nthWeekStartTime :: UTCTime -> UTCTime -> Int -> UTCTime
nthWeekStartTime scheduleStartTime currentTime n =
    if weeksUntilNext == 0
        currentTime
    else
        nextOccurrence
    where   nextOccurence   = truncateWeek . dateToUTC $ addDays (7 * weeksUntilNext) currentDate
            currentDate     = utctDay currentTime
-}

nthDayStartTime :: UTCTime -> UTCTime -> Integer -> UTCTime
nthDayStartTime scheduleStartTime currentTime n =
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

dayOfMonthStartTime :: UTCTime -> Int -> UTCTime
dayOfMonthStartTime currentTime targetDay =
    if daysUntilNext == 0
    then
        currentTime
    else 
        nextOccurrence
    where   nextOccurrence                          = dayOfMonthStartTime possibleNextOccurrence targetDay
            possibleNextOccurrence                  = dateToUTC $ addDays (toInteger daysUntilNext) currentDate
            daysUntilNext                           = (targetDay - currentDay) `mod` daysInMonth
            (currentYear, currentMonth, currentDay) = toGregorian $ utctDay currentTime
            currentDate                             = utctDay currentTime
            daysInMonth                             = gregorianMonthLength currentYear currentMonth
