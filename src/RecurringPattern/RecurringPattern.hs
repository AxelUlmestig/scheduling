{-# LANGUAGE ExistentialQuantification #-}

module RecurringPattern.RecurringPattern (
    RecurringPattern(..),
    UnitSize(..),
    RPWrapper(..),
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
    startTime       :: UTCTime -> a -> Maybe UTCTime
    endTime         :: UTCTime -> a -> Maybe UTCTime

data RPWrapper = forall a. RecurringPattern a => RPWrapper a

instance RecurringPattern RPWrapper where
    unitSize (RPWrapper rp)                 = unitSize rp
    startTime currentTime (RPWrapper rp)    = startTime currentTime rp
    endTime currentTime (RPWrapper rp)      = endTime currentTime rp

nextStartTime :: RecurringPattern a => UTCTime -> UTCTime -> a -> Maybe UTCTime
nextStartTime scheduleStartTime currentTime recurringPattern
    | scheduleStartTime > currentTime   = startTime scheduleStartTime recurringPattern
    | otherwise                         = startTime currentTime recurringPattern

nextEndTime :: RecurringPattern a => UTCTime -> UTCTime -> a -> Maybe UTCTime
nextEndTime scheduleStartTime currentTime recurringPattern =
    --flip endTime recurringPattern <$> nextStartTime scheduleStartTime currentTime recurringPattern
    nextStartTime scheduleStartTime currentTime recurringPattern >>= flip endTime recurringPattern

compareUnitSize :: RecurringPattern a => a -> a -> Ordering
compareUnitSize = compare `on` unitSize

sameUnitSize :: RecurringPattern a => a -> a -> Bool
sameUnitSize = (==) `on` unitSize
