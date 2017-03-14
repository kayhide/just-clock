module JustClock
    ( main
    ) where

import Data.Time.Clock
import Data.Time.LocalTime
import Reflex.Dom

import qualified JustClock.Widget as W

main :: IO ()
main = do
  startTime <- getCurrentTime
  zone <- getCurrentTimeZone
  mainWidget $ do
    tick <- tickLossy 0.05 startTime
    now <- holdDyn startTime $ _tickInfo_lastUTC <$> tick
    sec <- mapDyn (floor . utctDayTime) now
    now' <- holdDyn startTime $ tagDyn now $ updated $ nubDyn sec
    el "div" $ do
      text "count: "
      display =<< count tick
    el "div" $ do
      text "time: "
      display now
    el "div" $ do
      text "time: "
      display now'
    t <- mapDyn (utcToLocalTime zone) now'
    tod <- mapDyn localTimeOfDay t
    el "div" $ do
      text "local time: "
      display t
    el "div" $ do
      displayTimeOfDay tod
    W.displayClock tod
    return ()

displayTimeOfDay :: MonadWidget t m => Dynamic t TimeOfDay -> m ()
displayTimeOfDay tod = do
  text "hour: "
  display =<< mapDyn todHour tod
  text ", min: "
  display =<< mapDyn todMin tod
  text ", sec: "
  display =<< mapDyn todSec tod
