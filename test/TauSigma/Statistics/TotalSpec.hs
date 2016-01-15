{-# LANGUAGE ScopedTypeVariables #-}

module TauSigma.Statistics.TotalSpec where

import qualified Data.Vector.Unboxed as U

import Pipes

import TauSigma.Statistics.Total (totdevs, (!*))
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

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)


spec :: Spec
spec = do
  describe "reflection" $
    it "A reflected time sequence is defined correctly at all of its range" $
      property prop_reflection
      
  describe "totdev" (mapM_ slopeTest totdevCases)

prop_reflection :: Int -> Bool
prop_reflection size = map (xs!*) xs' == xs'
  where xs :: U.Vector Int
        xs = U.generate size id
        xs' :: [Int]
        xs' = [2 - size .. 2*size - 3]


totdevCases :: [TestCase]
totdevCases =
  [ TestCase { name        = "wpm"
             , description = "Slope of white phase noise ~ -1.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-1.0)
             , tolerance   = 0.05
             , statistic   = totdevs 1
             , noise       = wpm
             }
  , TestCase { name        = "fpm"
             , description = "Slope of flicker phase noise ~ -1.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-1.0)
             , tolerance   = 0.18
             , statistic   = totdevs 1
             , noise       = fpm
             }
  , TestCase { name        = "wfm"
             , description = "Slope of white frequency noise ~ -0.5"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (-0.5)
             , tolerance   = 0.05
             , statistic   = totdevs 1
             , noise       = wfm
             }
  , TestCase { name        = "ffm"
             , description = "Slope of flicker frequency noise ~ 0.0"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (0.0)
             , tolerance   = 0.05
             , statistic   = totdevs 1
             , noise       = ffm
             }
  , TestCase { name        = "rwfm"
             , description = "Slope of random walk frequency noise ~ 0.5"
             , samples     = sampleSize
             , taus        = (10, 20)
             , expected    = (0.5)
             , tolerance   = 0.05
             , statistic   = totdevs 1
             , noise       = rwfm
             }
  ]
  where sampleSize = 200000
        wpm = whitePhase 1.0
        fpm = flickerPhase (octaves sampleSize) 1.0
        wfm = whiteFrequency 1.0 >-> toPhase
        ffm = flickerFrequency (octaves sampleSize) 1.0 >-> toPhase
        rwfm = randomWalkFrequency 1.0  >-> toPhase
