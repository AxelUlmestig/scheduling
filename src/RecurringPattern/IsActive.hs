
module RecurringPattern.IsActive (
    isActive,
    singleIsActive
) where

import Data.Time
import Data.List (groupBy, sortBy)

import RecurringPattern.RecurringPattern

isActive :: RecurringPattern a => UTCTime -> UTCTime -> [a] -> Bool
isActive scheduleStartTime currentTime =
    and . map layerIsActive . groupBy sameUnitSize
        where layerIsActive = or . map (singleIsActive scheduleStartTime currentTime)

singleIsActive :: RecurringPattern a => UTCTime -> UTCTime -> a -> Bool
singleIsActive scheduleStartTime currentTime =
    (== Just currentTime) . nextStartTime scheduleStartTime currentTime
