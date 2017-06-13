{-# LANGUAGE OverloadedStrings #-}

module JustClock.Widget
  ( displayClock
  ) where

import Data.Monoid
import Data.Text as Text
import Data.Map (Map, fromList)
import Data.Time.Clock
import Data.Time.LocalTime
import Reflex.Dom

svgDynAttr' :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
svgDynAttr' name attrs m = elDynAttrNS' ns name attrs m
  where ns = Just "http://www.w3.org/2000/svg"

svgDynAttr :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr name attrs m = do
  _ <- svgDynAttr' name attrs m
  m

svgAttr' :: MonadWidget t m => Text -> Map Text Text -> m a -> m (El t, a)
svgAttr' name attrs m = svgDynAttr' name (constDyn attrs) m

svgAttr :: MonadWidget t m => Text -> Map Text Text -> m a -> m a
svgAttr name attrs m = svgDynAttr name (constDyn attrs) m

width :: Double
width = 400

height :: Double
height = 400 :: Double

svgAttrs :: Map Text Text
svgAttrs = fromList
  [ ("viewBox", box)
  , ("width", (Text.pack . show) width)
  , ("height", (Text.pack . show) height)
  ]
  where box = (Text.pack . show) (-width / 2)
              <> " " <> (Text.pack . show) (-height /2)
              <> " " <> (Text.pack . show) width
              <> " " <> (Text.pack . show) height

type Circle = ((Text, Double), (Double, Double))

showCircle :: MonadWidget t m => Circle -> Int -> m ()
showCircle ((color, radius), (x, y)) degree = do
  svgAttr "circle" circleAttrs $ return ()
  where circleAttrs = fromList
          [ ("cx", (Text.pack . show) x)
          , ("cy", (Text.pack . show) y)
          , ("r", (Text.pack . show) radius)
          , ("style", "fill:" <> color)
          , ("transform", "rotate(" <> (Text.pack . show) degree <> ")")
          ]

displayClock :: MonadWidget t m => Dynamic t TimeOfDay -> m ()
displayClock tod = do
  let hour = hourDegree <$> tod
      min = minDegree <$> tod
      sec = secDegree <$> tod
  svgAttr "svg" svgAttrs $ do
    showCircle (("Black", 10), (0, 0)) 0
    mapM_ (showCircle (("Lime", 2), (0, -100))) $ (30*) <$> [0..11]
    dyn $ (showCircle (("Black", 5), (0, -80))) <$> sec
    dyn $ (showCircle (("Cyan", 8), (0, -60))) <$> min
    dyn $ (showCircle (("Blue", 10), (0, -30))) <$> hour
    return ()
  where hourDegree tod = 360 `div` 12 * (todHour tod `mod` 12)
        minDegree tod = 360 `div` 60 * (todMin tod)
        secDegree tod = 360 `div` 60 * floor (todSec tod)
