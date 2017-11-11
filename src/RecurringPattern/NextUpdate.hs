
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

nextUpdate :: RecurringPattern a => UTCTime -> UTCTime -> [a] -> Maybe UTCTime
nextUpdate scheduleStartTime currentTime []                 = Nothing
nextUpdate scheduleStartTime currentTime recurringPatterns  =
    if isActive scheduleStartTime currentTime recurringPatterns
    then
        nextEndTimeInternal scheduleStartTime currentTime recurringPatternLayers
    else
        nextStartTimeInternal scheduleStartTime currentTime recurringPatternLayers
    where recurringPatternLayers = groupByUnitSize recurringPatterns

groupByUnitSize :: RecurringPattern a => [a] -> [[a]]
groupByUnitSize = groupBy sameUnitSize . sortBy compareUnitSize

nextStartTimeInternal :: RecurringPattern a => UTCTime -> UTCTime -> [[a]] -> Maybe UTCTime
nextStartTimeInternal _ currentTime []                                          = Just currentTime
nextStartTimeInternal scheduleStartTime currentTime (currentLayer:lowerLayers)  = do
    let earliestInLayer         = minimumBy (compareMaybeEarliest `on` getStartTime) currentLayer
    startTime                   <- getStartTime earliestInLayer
    let currentLayerEndTime     = maybe endOfTime id $ endTime currentTime earliestInLayer
    nextLayerStartTime          <- nextStartTimeInternal scheduleStartTime startTime lowerLayers
    if nextLayerStartTime < currentLayerEndTime
    then
        Just nextLayerStartTime
    else
        nextStartTimeInternal scheduleStartTime currentLayerEndTime (currentLayer:lowerLayers)
    where   getStartTime = nextStartTime scheduleStartTime currentTime

compareMaybeEarliest :: Maybe UTCTime -> Maybe UTCTime -> Ordering
compareMaybeEarliest t1 Nothing            = LT
compareMaybeEarliest Nothing t2            = GT
compareMaybeEarliest (Just t1) (Just t2)   = compare t1 t2

nextEndTimeInternal :: RecurringPattern a => UTCTime -> UTCTime -> [[a]] -> Maybe UTCTime
nextEndTimeInternal scheduleStartTime currentTime
    = minimumBy (compare `on` maybe endOfTime id)
    . (Nothing:)
    . map (latestEndTime loopLimit scheduleStartTime currentTime)

latestEndTime :: RecurringPattern a => Int -> UTCTime -> UTCTime -> [a] -> Maybe UTCTime
latestEndTime loopCounter scheduleStartTime currentTime recurringPatterns
    | loopCounter <= 0  = Nothing
    | otherwise         = do
        latestEndTimeInLayer <- foldl latestMaybeTime (Just currentTime) . map (endTime currentTime) . filter isRelevant $ recurringPatterns
        if latestEndTimeInLayer == currentTime
        then
            Just currentTime
        else
            latestEndTime (loopCounter - 1) scheduleStartTime latestEndTimeInLayer recurringPatterns
    where   isRelevant = singleIsActive scheduleStartTime currentTime

latestMaybeTime :: Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime
latestMaybeTime t1 t2 = max <$> t1 <*> t2
