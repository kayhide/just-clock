module JustClock.Widget
  ( displayClock
  ) where

import Data.Map (Map, fromList)
import Data.Time.Clock
import Data.Time.LocalTime
import Reflex.Dom

svgDynAttr' :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
svgDynAttr' name attrs m = elDynAttrNS' ns name attrs m
  where ns = Just "http://www.w3.org/2000/svg"

svgDynAttr :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
svgDynAttr name attrs m = do
  _ <- svgDynAttr' name attrs m
  m

svgAttr' :: MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
svgAttr' name attrs m = svgDynAttr' name (constDyn attrs) m

svgAttr :: MonadWidget t m => String -> Map String String -> m a -> m a
svgAttr name attrs m = svgDynAttr name (constDyn attrs) m

width :: Double
width = 400

height :: Double
height = 400 :: Double

svgAttrs :: Map String String
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
  svgAttr "circle" circleAttrs $ return ()
  where circleAttrs = fromList [ ("cx", show x)
                               , ("cy", show y)
                               , ("r", show radius)
                               , ("style", "fill:" ++ color)
                               , ("transform", "rotate(" ++ show degree ++ ")")
                               ]

displayClock :: MonadWidget t m => Dynamic t TimeOfDay -> m ()
displayClock tod = do
  hour <- mapDyn hourDegree tod
  min <- mapDyn minDegree tod
  sec <- mapDyn secDegree tod
  svgAttr "svg" svgAttrs $ do
    showCircle (("Black", 10), (0, 0)) 0
    mapM_ (showCircle (("Lime", 2), (0, -100))) $ map (30*) [0..11]
    _ <- dyn =<< mapDyn (showCircle (("Black", 5), (0, -80))) sec
    _ <- dyn =<< mapDyn (showCircle (("Cyan", 8), (0, -60))) min
    _ <- dyn =<< mapDyn (showCircle (("Blue", 10), (0, -30))) hour
    return ()
  where hourDegree tod = 360 `div` 12 * (todHour tod `mod` 12)
        minDegree tod = 360 `div` 60 * (todMin tod)
        secDegree tod = 360 `div` 60 * floor (todSec tod)
