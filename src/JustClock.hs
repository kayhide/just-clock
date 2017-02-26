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
    tick <- tickLossy 0.1 startTime
    now <- holdDyn startTime $ _tickInfo_lastUTC <$> tick
    el "div" $ do
      text "count: "
      display =<< count tick
    el "div" $ do
      text "time: "
      display now
    sec <- mapDyn (floor . utctDayTime) now
    now' <- holdDyn startTime $ tagDyn now $ updated $ nubDyn sec
    el "div" $ do
      text "time: "
      display now'
    t <- mapDyn (utcToLocalTime zone) now'
    tod <- mapDyn localTimeOfDay t
    el "div" $ do
      text "local time: "
      display t
    el "div" $ do
      _ <- dyn =<< mapDyn displayTimeOfDay tod
      return ()
    _ <- dyn =<< mapDyn W.clock tod
    return ()

displayTimeOfDay :: MonadWidget t m => TimeOfDay -> m ()
displayTimeOfDay tod = do
  text "hour: "
  text $ show $ todHour tod
  text ", min: "
  text $ show $ todMin tod
  text ", sec: "
  text $ show $ todSec tod
