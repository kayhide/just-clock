{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Reflex.Dom
import Control.Monad
import Data.Time.Clock as Time

someFunc = do
  startTime <- Time.getCurrentTime
  mainWidget $ do
    tick <- tickLossy 1 startTime
    now <- foldDyn (const._tickInfo_lastUTC) startTime tick
    el "div" $ do
      text "count: "
      display =<< count tick
    el "div" $ do
      text "time: "
      display now
