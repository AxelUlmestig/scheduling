
module TimeUtil (
    dateToUTC,
    truncateYear,
    truncateMonth,
    truncateDay,
    endOfTime
) where

import Data.Time

dateToUTC :: Day -> UTCTime
dateToUTC = flip UTCTime 0

truncateYear :: UTCTime -> UTCTime
truncateYear time = UTCTime truncatedDate 0
    where   (year, month, _)    = toGregorian (utctDay time)
            truncatedDate       = fromGregorian year 1 1

truncateMonth :: UTCTime -> UTCTime
truncateMonth time = UTCTime (fromGregorian year month 1) 0
    where   (year, month, _) = toGregorian (utctDay time)

{-
truncateWeek :: UTCTime -> UTCTime
-}

truncateDay :: UTCTime -> UTCTime
truncateDay time = UTCTime (fromGregorian year month day) 0
    where   (year, month, day) = toGregorian (utctDay time)

endOfTime :: UTCTime
endOfTime = UTCTime (fromGregorian 9999 21 31) 0
