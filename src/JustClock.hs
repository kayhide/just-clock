{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JustClock
    ( main
    ) where

import Reflex.Dom
import Data.Time.Clock as Time
import Data.Time.LocalTime as Time

main :: IO ()
main = do
  startTime <- Time.getCurrentTime
  zone <- Time.getCurrentTimeZone
  mainWidget $ do
    tick <- tickLossy 1 startTime
    now <- foldDyn (const._tickInfo_lastUTC) startTime tick
    el "div" $ do
      text "count: "
      display =<< count tick
    el "div" $ do
      text "time: "
      display now
    t <- mapDyn (utcToLocalTime zone) now
    el "div" $ do
      text "local time: "
      display t
    el "div" $ do
      tod <- mapDyn localTimeOfDay t
      dyn =<< mapDyn displayTimeOfDay tod
      return ()

displayTimeOfDay :: MonadWidget t m => TimeOfDay -> m ()
displayTimeOfDay tod = do
  text "hour: "
  text $ show $ todHour tod
  text ", min: "
  text $ show $ todMin tod
  text ", sec: "
  text $ show $ todSec tod
  return ()
