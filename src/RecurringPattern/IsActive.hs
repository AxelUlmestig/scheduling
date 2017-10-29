
module RecurringPattern.IsActive (
    isActive,
    singleIsActive
) where

import Data.Time
import Data.List (groupBy, sort)

import RecurringPattern.RecurringPattern

isActive :: UTCTime -> UTCTime -> [RecurringPattern] -> Bool
isActive scheduleStartTime currentTime =
    and . map layerIsActive . groupBy sameUnitSize . sort
        where layerIsActive = or . map (singleIsActive scheduleStartTime currentTime)

singleIsActive :: UTCTime -> UTCTime -> RecurringPattern -> Bool
singleIsActive scheduleStartTime currentTime =
    (== currentTime) . recurringPatternNextStartTime scheduleStartTime currentTime
