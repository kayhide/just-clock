{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JustClock
    ( main
    ) where

import Data.Map (Map, fromList)
import Data.Time.Clock
import Data.Time.LocalTime
import Reflex.Dom

main :: IO ()
main = do
  startTime <- getCurrentTime
  zone <- getCurrentTimeZone
  mainWidget $ do
    tick <- tickLossy 1 startTime
    now <- holdDyn startTime $ _tickInfo_lastUTC <$> tick
    el "div" $ do
      text "count: "
      display =<< count tick
    el "div" $ do
      text "time: "
      display now
    t <- mapDyn (utcToLocalTime zone) now
    tod <- mapDyn localTimeOfDay t
    el "div" $ do
      text "local time: "
      display t
    el "div" $ do
      _ <- dyn =<< mapDyn displayTimeOfDay tod
      return ()
    _ <- dyn =<< mapDyn displayClock tod
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


svgElDynAttr' :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
svgElDynAttr' el attrs m = elDynAttrNS' ns el attrs m
  where ns = Just "http://www.w3.org/2000/svg"

svgElDynAttr :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m ()
svgElDynAttr el attrs m = do
  svgElDynAttr' el attrs m
  return ()

width = 400 :: Double
height = 400 :: Double
svgAttrs = fromList [ ("viewBox", box)
                    , ("width", show width)
                    , ("height", show height)
                    ]
  where box = show (-width / 2)
              ++ " " ++ show (-height /2)
              ++ " " ++ show width
              ++ " " ++ show height

type Circle = ((String, Double), (Double, Double))

showCircle :: MonadWidget t m => Circle -> Int -> m ()
showCircle ((color, radius), (x, y)) degree = do
  svgElDynAttr "circle" (constDyn circleAttrs) $ return ()
  where circleAttrs = fromList [ ("cx", show x)
                               , ("cy", show y)
                               , ("r", show radius)
                               , ("style", "fill:" ++ color)
                               , ("transform", "rotate(" ++ show degree ++ ")")
                               ]

displayClock :: MonadWidget t m => TimeOfDay -> m ()
displayClock tod = do
  svgElDynAttr "svg" (constDyn svgAttrs) $ do
    showCircle (("Black", 10), (0, 0)) 0
    showCircle (("Black", 5), (0, -80)) secDegree
    showCircle (("Cyan", 8), (0, -60)) minDegree
    showCircle (("Blue", 10), (0, -30)) hourDegree
    mapM_ (showCircle (("Lime", 2), (0, -100))) $ map (30*) [0..11]
  where hourDegree = 360 `div` 12 * (todHour tod `mod` 12)
        minDegree = 360 `div` 60 * (todMin tod)
        secDegree = 360 `div` 60 * floor (todSec tod)
