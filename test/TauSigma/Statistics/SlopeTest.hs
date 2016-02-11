{-# LANGUAGE RecordWildCards #-}

-- | Utility to run tau-sigma slope tests.  
module TauSigma.Statistics.SlopeTest
       ( Statistic
       , Slope
       , Error
       , TestCase(..)
       , slopeTest
       ) where

import Data.Tagged
import Data.Vector.Unboxed (Vector)

import Data.Random (RVarT, runRVarT)
import Data.Random.Source.MWC (create)
    
import Pipes
import qualified Pipes.Prelude as P

import Text.Printf

import TauSigma.Statistics.Types (Tau, TauSigma)
import TauSigma.Util.Pipes.Noise (TimeData)
import TauSigma.Util.Vector (takeVector)

import Test.Hspec


type Statistic = Vector Double -> [TauSigma Double]
type Slope = Double
type Error = Double

data TestCase
  = TestCase { name        :: String
             , description :: String
             , samples     :: Int
             , taus        :: (Double, Double)
             , expected    :: Slope
             , tolerance   :: Error
             , statistic   :: Statistic
             , noise       :: Producer (TimeData Double) (RVarT IO) ()
             }

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

runWithCreate :: RVarT IO a -> IO a
runWithCreate ma = runRVarT ma =<< create

filterKeys :: Ord a => (Tau a, Tau a) -> [TauSigma a] -> [TauSigma a]
filterKeys (lo,hi) = filter go
  where go (tau, _) = lo <= tau && tau <= hi 

badSlopes
  :: Slope
  -> Error
  -> [TauSigma Double]
  -> [(Double, Double, Slope, Error)]
badSlopes standard tolerance graph =
  [ (x0, x1, round slope, round err)
  | (p0@(x0, _), p1@(x1, _)) <- withSuccessors (,) graph
  , let slope = logSlope p0 p1
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
                

