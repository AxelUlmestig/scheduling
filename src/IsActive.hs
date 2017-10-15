
module IsActive (
    isActive,
    singleIsActive
) where

import Data.Time
import Data.List (groupBy, sortBy)

import RecurringPattern

isActive :: UTCTime -> UTCTime -> [RecurringPattern] -> Bool
isActive scheduleStartTime currentTime recurringPatterns =
    and . map layerIsActive . groupBy sameUnitSize . sortBy compareUnitSize $ recurringPatterns
        where layerIsActive = or . map (singleIsActive scheduleStartTime currentTime)

compareUnitSize :: RecurringPattern -> RecurringPattern -> Ordering
compareUnitSize rp1 rp2 = compare (unitSize rp1) (unitSize rp2)

singleIsActive :: UTCTime -> UTCTime -> RecurringPattern -> Bool
singleIsActive scheduleStartTime currentTime recurringPattern =
    recurringPatternNextStartTime scheduleStartTime currentTime recurringPattern == currentTime

sameUnitSize :: RecurringPattern -> RecurringPattern -> Bool
sameUnitSize rp1 rp2 = unitSize rp1 == unitSize rp2
