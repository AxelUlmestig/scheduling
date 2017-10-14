
module Schedule (

) where

data Schedule = Schedule UTCTime UTCTime [RecurringPattern]
