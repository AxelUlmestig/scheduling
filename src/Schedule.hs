
module Schedule (
    Schedule(..),
    isActive,
    nextUpdate
) where

import TimeUtil (endOfTime)
import qualified RecurringPattern

data Schedule = Schedule UTCTime UTCTime [RecurringPattern]

endTime :: Schdedule -> UTCTime
endTime (Schedule _ scheduleEndTime _) = scheduleEndTime

nextUpdate :: Schedule -> UTCTime -> UTCTime
nextUpdate schedule currentTime
    | isActive schedule                 = nextUpdateActive schedule currentTime
    | otherwise                         = nextUpdateInactive schedule currentTime

nextUpdateActive :: schedule -> utctime -> utctime
nextUpdateActive schedule currenttime
    | nexttime > endtime schedule   = endtime schedule
    | otherwise                     = nexttime
    where   nexttime = nextupdateinternal schedule

nextUpdateInactive :: schedule -> utctime -> utctime
nextUpdateInactive schedule currenttime
    | nexttime > endtime schedule   = endOfTime
    | otherwise                     = nexttime
    where   nexttime = nextupdateinternal schedule

nextUpdateInternal :: Schedule -> UTCTime -> UTCTime
nextUpdateInternal (Schedule scheduleStartTime scheduleEndTime recurringPatterns) =
    RecurringPattern.nextUpdate scheduleStartTime scheduleEndTime recurringPatterns

isActive :: Schedule -> UTCTime -> Bool
isActive (Schedule scheduleStartTime scheduleEndTime recurringPatterns) currentTime
    | currentTime >= scheduleEndTime    = False
    | otherwise                         = calculatedState
    where calculatedState = RecurringPattern.IsActive scheduleStartTime currentTime recurringPatterns
