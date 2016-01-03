module TauSigma.Statistics.HadamardSpec where

import Data.IntMap.Lazy (IntMap)

import Pipes

import TauSigma.Statistics.Hadamard (hdevs)
import TauSigma.Statistics.SlopeTest

import TauSigma.Util.Pipes.Noise
  ( whitePhase
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
  describe "hdev" (mapM_ slopeTest hdevCases)

hdevCases :: [TestCase]
hdevCases =
  [ TestCase { name        = "wpm"
             , description = "Slope of white phase noise ~ -1.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-1.0)
             , tolerance   = 0.1
             , statistic   = hdevs 1
             , noise       = wpm
             }
  , TestCase { name        = "fpm"
             , description = "Slope of flicker phase noise ~ -1.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-1.0)
             , tolerance   = 0.2
             , statistic   = hdevs 1
             , noise       = fpm
             }
  , TestCase { name        = "wfm"
             , description = "Slope of white frequency noise ~ -0.5"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-0.5)
             , tolerance   = 0.05
             , statistic   = hdevs 1
             , noise       = wfm
             }
  , TestCase { name        = "ffm"
             , description = "Slope of flicker frequency noise ~ 0.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (0.0)
             , tolerance   = 0.05
             , statistic   = hdevs 1
             , noise       = ffm
             }
  , TestCase { name        = "rwfm"
             , description = "Slope of random walk frequency noise ~ 0.5"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (0.5)
             , tolerance   = 0.05
             , statistic   = hdevs 1
             , noise       = rwfm
             }
  ]
  where sampleSize = 200000
        wpm = whitePhase 1.0
        fpm = flickerPhase (octaves sampleSize) 1.0
        wfm = whiteFrequency 1.0 >-> toPhase
        ffm = flickerFrequency (octaves sampleSize) 1.0 >-> toPhase
        rwfm = randomWalkFrequency 1.0  >-> toPhase
