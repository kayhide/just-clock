{-# LANGUAGE OverloadedStrings #-}

module JustClock
    ( main
    ) where

import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as Map
import Reflex.Dom

import qualified JustClock.Widget as W

headElement :: MonadWidget t m => m ()
headElement = do
  elAttr "meta" (Map.singleton "charset" "utf-8") $ return ()
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  stylesheet "https://code.getmdl.io/1.3.0/material.indigo-green.min.css"
  script "https://code.getmdl.io/1.3.0/material.min.js"
  return ()
  where
    stylesheet href = elAttr "link"
                      (Map.fromList [("rel", "stylesheet"), ("href", href)]) $
                      return ()
    script src = elAttr "script"
                 (Map.fromList [("src", src), ("defer", "defer")]) $
                 return ()

main :: IO ()
main = do
  startTime <- getCurrentTime
  zone <- getCurrentTimeZone
  mainWidgetWithHead headElement $ do
    tick <- tickLossy 0.05 startTime
    now <- holdDyn startTime $ _tickInfo_lastUTC <$> tick
    let sec = floor . utctDayTime <$> now
    now' <- holdDyn startTime $ tagPromptlyDyn now $ updated $ uniqDyn sec
    let t = utcToLocalTime zone <$> now'
        tod = localTimeOfDay <$> t
    el "div" $ text "count: " >> count tick >>= display
    el "div" $ text "time: " >> display now
    el "div" $ text "time: " >> display now'
    el "div" $ text "local time: " >> display t
    el "div" $ displayTimeOfDay tod
    W.displayClock tod
    el "div" $ do
      linkClass "Push me" "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--accent"
      linkClass "And me too" "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--colored"
      elClass "button" "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--raised mdl-button--colored" $ do
        elClass "i" "material-icons" $ text "add"
      elClass "button" "mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--fab mdl-button--colored" $ do
        elClass "i" "material-icons" $ text "add"
    return ()

displayTimeOfDay :: MonadWidget t m => Dynamic t TimeOfDay -> m ()
displayTimeOfDay tod = do
  text "hour: " >> display (todHour <$> tod)
  text ", min: " >> display (todMin <$> tod)
  text ", sec: " >> display (todSec <$> tod)
