{-# LANGUAGE RecordWildCards #-}

module TauSigma.Statistics.Theo1Spec where

import Data.IntMap.Lazy (IntMap)

import Pipes

import TauSigma.Statistics.Theo1 (theo1devs, theoBRdevs)
import TauSigma.Statistics.SlopeTest

import TauSigma.Util.Pipes.Noise
  ( TimeData
  , whitePhase
  , flickerPhase
  , whiteFrequency
  , flickerFrequency
  , randomWalkFrequency
  , toPhase
  , octaves
  )

import Test.Hspec


spec :: Spec
spec = do
  describe "theo1" (mapM_ slopeTest theo1Cases)
  describe "theoBR" (mapM_ slopeTest theoBRCases)

theo1Cases :: [TestCase]
theo1Cases =
  [ TestCase { name        = "wfm"
             , description = "Slope of white frequency noise ~ -0.5"
             , samples     = sampleSize
             , taus        = (30, 50)
             , expected    = (-0.5)
             , tolerance   = 0.1
             , statistic   = theo1devs 1
             , noise       = wfm
             }
  ]
  where sampleSize = 3500
        wfm = whiteFrequency 1.0 >-> toPhase


theoBRCases :: [TestCase]
theoBRCases =
  [ TestCase { name        = "wfm"
             , description = "Slope of white frequency noise ~ -0.5"
             , samples     = sampleSize
             , taus        = (30, 50)
             , expected    = (-0.5)
             , tolerance   = 0.1
             , statistic   = theoBRdevs 1
             , noise       = wfm
             }
  , TestCase { name        = "ffm"
             , description = "Slope of flicker frequency noise ~ 0.0"
             , samples     = sampleSize
             , taus        = (30, 50)
             , expected    = (0.0)
             , tolerance   = 0.25
             , statistic   = theoBRdevs 1
             , noise       = ffm
             }
  , TestCase { name        = "rwfm"
             , description = "Slope of random walk frequency noise ~ 0.5"
             , samples     = sampleSize
             , taus        = (30, 50)
             , expected    = (0.5)
             , tolerance   = 0.05
             , statistic   = theoBRdevs 1
             , noise       = rwfm
             }
  ]
  where sampleSize = 3500
        wfm = whiteFrequency 1.0 >-> toPhase
        ffm = flickerFrequency (octaves sampleSize) 1.0 >-> toPhase
        rwfm = randomWalkFrequency 1.0  >-> toPhase


