
module RecurringPattern.RecurringPattern (
    RecurringPattern(..),
    UnitSize(..),
    nextStartTime,
    nextEndTime,
    sameUnitSize,
    compareUnitSize
) where

import Data.Time
import Data.Function (on)

import TimeUtil

data UnitSize =
    Year    |
    Month   |
    Week    |
    Day     |
    Second
    deriving (Eq, Show, Ord)

class RecurringPattern a where
    unitSize        :: a -> UnitSize
    startTime       :: UTCTime -> UTCTime -> a -> UTCTime
    endTime         :: UTCTime -> a -> UTCTime
    isInfiniteLoop  :: UTCTime -> a -> UTCTime -> Bool

nextStartTime :: RecurringPattern a => UTCTime -> UTCTime -> a -> UTCTime
nextStartTime scheduleStartTime currentTime recurringPattern
    | scheduleStartTime > currentTime   = startTime scheduleStartTime scheduleStartTime recurringPattern
    | otherwise                         = startTime scheduleStartTime currentTime recurringPattern

nextEndTime :: RecurringPattern a => UTCTime -> UTCTime -> a -> UTCTime
nextEndTime scheduleStartTime currentTime recurringPattern =
    flip endTime recurringPattern $ nextStartTime scheduleStartTime currentTime recurringPattern

compareUnitSize :: RecurringPattern a => a -> a -> Ordering
compareUnitSize = compare `on` unitSize

sameUnitSize :: RecurringPattern a => a -> a -> Bool
sameUnitSize = (==) `on` unitSize
