{-# LANGUAGE ScopedTypeVariables #-}

module TauSigma.Util.AllanSpec where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Data.Vector (Vector)

import Pipes

import System.Random.MWC.Monad (Rand, runWithCreate)

import TauSigma.Util.Allan (adevs)
import TauSigma.Util.Pipes.Noise (white, flicker, octaves, brown, integrate)
import TauSigma.Util.Vector (takeVector)

import Test.Hspec

spec :: Spec
spec = do
  describe "wpm slope" $ do
    it "The slope of white phase noise should be about -1.0" $ do
      slopeTest 200000 (10, 20) (-1.0) 0.05 (white 1.0)

  describe "fpm slope" $ do
    it "The slope of flicker phase noise should be about -1.0" $ do
      slopeTest 200000 (10, 20) (-1.0) 0.05 (white 1.0)

  describe "wfm slope" $ do
    it "The slope of white frequency noise should be about -0.5" $ do
      slopeTest 200000 (10, 20) (-0.5) 0.05 (white 1.0 >-> integrate)

  describe "ffm slope" $ do
    it "The slope of flicker frequency noise should be about 0.0" $ do
      let noise = flicker (octaves 100000) 1.0 >-> integrate
      slopeTest 200000 (10, 20) (0.0) 0.025 noise 

  describe "rwfm slope" $ do
    it "The slope of random walk frequency noise should be about 0.5" $ do
      slopeTest 1000000 (10, 20) (0.5) 0.01 (brown 1.0 >-> integrate)


type Point = (Int, Double)
type Slope = Double
type Error = Double

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
slopeTest
  :: Int         -- ^ The number of noise samples to take
  -> (Int, Int)  -- ^ The sampling intervals at which to check the slopes
  -> Slope       -- ^ The expected slope
  -> Error       -- ^ The amount of error tolerated
  -> Producer Double (Rand IO) ()
  -> IO ()
slopeTest samples taus standard tolerance source = do
  noise :: Vector Double <- runWithCreate (takeVector samples source)
  let graph = filterKeys taus (adevs 1 noise)
  let failures = badSlopes standard tolerance graph
  failures `shouldBe` []  

filterKeys :: (Int, Int) -> IntMap a -> IntMap a
filterKeys (lo,hi) = IntMap.filterWithKey go
  where go key _ = lo <= key && key <= hi 

badSlopes :: Slope -> Error -> IntMap Double -> [(Point, Point, Slope, Error)]
badSlopes standard tolerance graph =
  [ (p0, p1, slope, err)
  | let points = IntMap.toList graph
  , (p0@(x0, y0), p1@(x1, y1)) <- withSuccessors (,) points
  , let slope = logSlope (fromIntegral x0, y0) (fromIntegral x1, y1)
  , let err = abs (slope - standard)
  , err > tolerance
  ]

withSuccessors :: (a -> a -> r) -> [a] -> [r]
withSuccessors _ [] = []
withSuccessors f (a:as) = map (f a) as ++ withSuccessors f as

logSlope :: (Double, Double) -> (Double, Double) -> Double
logSlope (x0,y0) (x1,y1) = slope (log x0, log y0) (log x1, log y1)

slope :: (Double, Double) -> (Double, Double) -> Double
slope (x0,y0) (x1,y1) = (y1 - y0) / (x1 - x0)
                

