{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module TauSigma.Statistics.AllanSpec where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Tagged
import Data.Vector (Vector)

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC.Monad (Rand, runWithCreate)

import Text.Printf

import TauSigma.Statistics.Allan (adevs)
import TauSigma.Statistics.Theo1 (theoBRdevs)

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
import TauSigma.Util.Vector (takeVector)

import Test.Hspec


type Statistic = Vector Double -> IntMap Double
type Slope = Double
type Error = Double

data TestCase
  = TestCase { name        :: String
             , description :: String
             , samples     :: Int
             , taus        :: (Int, Int)
             , expected    :: Slope
             , tolerance   :: Error
             , statistic   :: Statistic
             , noise       :: Producer (TimeData Double) (Rand IO) ()
             }

spec :: Spec
spec = do
  describe "adev" (mapM_ slopeTest adevCases)

adevCases :: [TestCase]
adevCases =
  [ TestCase { name        = "wpm"
             , description = "Slope of white phase noise ~ -1.0"
             , samples     = 200000
             , taus        = (10, 20)
             , expected    = (-1.0)
             , tolerance   = 0.05
             , statistic   = adevs 1
             , noise       = whitePhase 1.0
             }
  , let len = 200000
    in TestCase { name        = "fpm"
                , description = "Slope of flicker phase noise ~ -1.0"
                , samples     = len
                , taus        = (10, 20)
                , expected    = (-1.0)
                , tolerance   = 0.18
                , statistic   = adevs 1
                , noise       = flickerPhase (octaves len) 1.0
                }
  , TestCase { name        = "wfm"
             , description = "Slope of white frequency noise ~ -0.5"
             , samples     = 200000
             , taus        = (10, 20)
             , expected    = (-0.5)
             , tolerance   = 0.05
             , statistic   = adevs 1
             , noise       = whiteFrequency 1.0 >-> toPhase
             }
  , let len = 200000
    in TestCase { name        = "ffm"
                , description = "Slope of flicker frequency noise ~ 0.0"
                , samples     = len
                , taus        = (10, 20)
                , expected    = (0.0)
                , tolerance   = 0.05
                , statistic   = adevs 1
                , noise       = flickerFrequency (octaves len) 1.0  >-> toPhase
                }
  , TestCase { name        = "rwfm"
             , description = "Slope of random walk frequency noise ~ 0.5"
             , samples     = 200000
             , taus        = (10, 20)
             , expected    = (0.5)
             , tolerance   = 0.05
             , statistic   = adevs 1
             , noise       = randomWalkFrequency 1.0  >-> toPhase
             }
  ]


-- | Validate that the slope of the tau/sigma plot is more or less
-- what it should be for the given noise source.
--
-- Note that testing at sampling intervals that are either too low or
-- too high tends to produce larger divergences from the expected
-- value.  That's what the second argument (the range of intervals) is
-- for; you want to pick "middle" values.
--
-- Also note that this test is based on 'runWithCreate' and as such is not
-- random at all.  This is a feature, not a bug, IMHO.
--
slopeTest :: TestCase -> Spec
slopeTest TestCase {..} =
  describe name $ do
    it description $ do
      input <- runWithCreate (takeVector samples (noise >-> P.map unTagged))
      let graph = filterKeys taus (statistic input)
      let failures = badSlopes expected tolerance graph
      failures `shouldBe` []


filterKeys :: (Int, Int) -> IntMap a -> IntMap a
filterKeys (lo,hi) = IntMap.filterWithKey go
  where go key _ = lo <= key && key <= hi 

type Point = (Int, Double)

badSlopes :: Slope -> Error -> IntMap Double -> [(Int, Int, Slope, Error)]
badSlopes standard tolerance graph =
  [ (x0, x1, round slope, round err)
  | let points = IntMap.toList graph
  , (p0@(x0, y0), p1@(x1, y1)) <- withSuccessors (,) points
  , let slope = logSlope (fromIntegral x0, y0) (fromIntegral x1, y1)
  , let err = abs (slope - standard)
  , err > tolerance
  ]
  where round :: Double -> Double
        round x = read (printf "%.5f" x)

withSuccessors :: (a -> a -> r) -> [a] -> [r]
withSuccessors _ [] = []
withSuccessors f (a:as) = map (f a) as ++ withSuccessors f as

logSlope :: (Double, Double) -> (Double, Double) -> Double
logSlope (x0,y0) (x1,y1) = slope (log x0, log y0) (log x1, log y1)
  where
    slope :: (Double, Double) -> (Double, Double) -> Double
    slope (x0,y0) (x1,y1) = (y1 - y0) / (x1 - x0)
                

