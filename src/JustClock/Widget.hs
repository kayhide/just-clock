module JustClock.Widget
  ( clock
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

clock :: MonadWidget t m => TimeOfDay -> m ()
clock tod = do
  svgAttr "svg" svgAttrs $ do
    showCircle (("Black", 10), (0, 0)) 0
    showCircle (("Black", 5), (0, -80)) secDegree
    showCircle (("Cyan", 8), (0, -60)) minDegree
    showCircle (("Blue", 10), (0, -30)) hourDegree
    mapM_ (showCircle (("Lime", 2), (0, -100))) $ map (30*) [0..11]
  where hourDegree = 360 `div` 12 * (todHour tod `mod` 12)
        minDegree = 360 `div` 60 * (todMin tod)
        secDegree = 360 `div` 60 * floor (todSec tod)
