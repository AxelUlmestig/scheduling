
module RecurringPattern.NextUpdate (
    nextUpdate
) where

import Data.Time
import Data.List (groupBy, maximumBy, minimumBy, sortBy)
import Data.Function (on)

import RecurringPattern.RecurringPattern
import RecurringPattern.IsActive
import TimeUtil

loopLimit = 100 :: Int

nextUpdate :: RecurringPattern a => UTCTime -> UTCTime -> [a] -> UTCTime
nextUpdate scheduleStartTime currentTime []                 = endOfTime
nextUpdate scheduleStartTime currentTime recurringPatterns  =
    if isActive scheduleStartTime currentTime recurringPatterns
    then
        nextEndTimeInternal scheduleStartTime currentTime recurringPatternLayers
    else
        nextStartTimeInternal scheduleStartTime currentTime recurringPatternLayers
    where recurringPatternLayers = sortByUnitSize recurringPatterns

sortByUnitSize :: RecurringPattern a => [a] -> [[a]]
sortByUnitSize = groupBy sameUnitSize . sortBy compareUnitSize

nextStartTimeInternal :: RecurringPattern a => UTCTime -> UTCTime -> [[a]] -> UTCTime
nextStartTimeInternal _ currentTime []                                           = currentTime
nextStartTimeInternal scheduleStartTime currentTime (currentLayer:lowerLayers)   =
    if nextLayerStartTime < currentLayerEndTime
    then
        nextLayerStartTime
    else
        nextStartTimeInternal scheduleStartTime currentLayerEndTime (currentLayer:lowerLayers)
    where   earliestInLayer     = minimumBy (compare `on` getStartTime) currentLayer
            getStartTime        = nextStartTime scheduleStartTime currentTime
            startTime           = getStartTime earliestInLayer
            currentLayerEndTime = endTime currentTime earliestInLayer
            nextLayerStartTime  = nextStartTimeInternal scheduleStartTime startTime lowerLayers

nextEndTimeInternal :: RecurringPattern a => UTCTime -> UTCTime -> [[a]] -> UTCTime
nextEndTimeInternal scheduleStartTime currentTime =
    minimum . map (latestEndTime loopLimit scheduleStartTime currentTime)

latestEndTime :: RecurringPattern a => Int -> UTCTime -> UTCTime -> [a] -> UTCTime
latestEndTime loopCounter scheduleStartTime currentTime recurringPatterns
    | loopCounter <= 0                      = endOfTime
    | latestEndTimeInLayer == currentTime   = currentTime
    | otherwise                             = latestEndTime (loopCounter - 1) scheduleStartTime latestEndTimeInLayer recurringPatterns
    where   latestEndTimeInLayer    = foldl max currentTime . map getEndTime . filter isRelevant $ recurringPatterns
            isRelevant              = singleIsActive scheduleStartTime currentTime
            getEndTime              = endTime currentTime
