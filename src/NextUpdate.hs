
module NextUpdate (
    nextUpdate
) where

import Data.Time
import Data.List (groupBy, maximumBy, minimumBy, sort, sortBy)
import Data.Function (on)

import RecurringPattern
import IsActive
import TimeUtil

nextUpdate :: UTCTime -> UTCTime -> [RecurringPattern] -> UTCTime
nextUpdate scheduleStartTime currentTime []                 = endOfTime
nextUpdate scheduleStartTime currentTime recurringPatterns  =
    if isActive scheduleStartTime currentTime recurringPatterns
    then
        nextEndTime scheduleStartTime currentTime recurringPatternLayers
    else
        nextStartTime scheduleStartTime currentTime recurringPatternLayers
    where recurringPatternLayers = sortByUnitSize recurringPatterns

sortByUnitSize :: [RecurringPattern] -> [[RecurringPattern]]
sortByUnitSize = groupBy sameUnitSize . sort

nextStartTime :: UTCTime -> UTCTime -> [[RecurringPattern]] -> UTCTime
nextStartTime _ currentTime []                                           = currentTime
nextStartTime scheduleStartTime currentTime (currentLayer:lowerLayers)   =
    if nextLayerStartTime < endTime
    then
        nextLayerStartTime
    else
        nextStartTime scheduleStartTime endTime (currentLayer:lowerLayers)
    where   earliestInLayer     = minimumBy (compare `on` getStartTime) currentLayer
            getStartTime        = recurringPatternNextStartTime scheduleStartTime currentTime
            startTime           = getStartTime earliestInLayer
            endTime             = recurringPatternNextEndTime scheduleStartTime currentTime earliestInLayer
            nextLayerStartTime  = nextStartTime scheduleStartTime startTime lowerLayers

nextEndTime :: UTCTime -> UTCTime -> [[RecurringPattern]] -> UTCTime
nextEndTime scheduleStartTime currentTime =
    minimum . map getLatestEndTime
    where   getLatestEndTime    = \recurringPatterns -> calculateEndTime (loopBreaker recurringPatterns) recurringPatterns
            calculateEndTime    = latestEndTime scheduleStartTime currentTime
            loopBreaker         = isInfiniteLoop scheduleStartTime . head

latestEndTime :: UTCTime -> UTCTime -> (UTCTime -> Bool) -> [RecurringPattern] -> UTCTime
latestEndTime scheduleStartTime currentTime isInfiniteLoop recurringPatterns
    | isInfiniteLoop currentTime            = endOfTime
    | latestEndTimeInLayer == currentTime   = currentTime
    | otherwise                             = latestEndTime scheduleStartTime latestEndTimeInLayer isInfiniteLoop recurringPatterns
    where   latestEndTimeInLayer    = foldl max currentTime . map getEndTime . filter isRelevant $ recurringPatterns
            isRelevant              = singleIsActive scheduleStartTime currentTime
            getEndTime              = recurringPatternNextEndTime scheduleStartTime currentTime

