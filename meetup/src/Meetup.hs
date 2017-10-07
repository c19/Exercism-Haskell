module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Debug.Trace (trace)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay First weekday year month = getFirst weekday firstday
  where firstday = fromGregorian year month 1
meetupDay Second weekday year month = addDays 7 day
  where day = meetupDay First weekday year month
meetupDay Third weekday year month = addDays 14 day
  where day = meetupDay First weekday year month
meetupDay Fourth weekday year month = addDays 21 day
  where day = meetupDay First weekday year month
meetupDay Last weekday year month = getLast weekday lastday
  where lastday = fromGregorian year month 31 -- don't worry, `fromGregorian` would clip it to last day
meetupDay Teenth weekday year month = getFirst weekday origin
  where origin = fromGregorian year month 13

getTeenth :: Weekday -> Int
getTeenth w = fromEnum w + 13

getLast :: Weekday -> Day -> Day
getLast w day = addDays delta day
  where origin = getWeekDay day
        target = fromEnum w
        delta  = toInteger $ -((-n) `mod` 7)
        n = target - origin - 7
getFirst :: Weekday -> Day -> Day
getFirst w day = addDays (toInteger ((7 + target - origin) `mod` 7)) day
  where origin = getWeekDay day
        target = fromEnum w

getWeekDay :: Day -> Int
getWeekDay day = w - 1
  where (y,n,w) = toWeekDate day
