{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Criterion.Main

import Control.Monad.Primitive (PrimMonad)
import Control.Parallel.Strategies (withStrategy, parBuffer, rdeepseq)

import Data.Tagged (Tagged(..))

import qualified Data.Vector.Unboxed as U

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC (create)
import System.Random.MWC.Probability (Prob, sample)

import TauSigma.Statistics.Types
import TauSigma.Statistics.Allan (adevs, mdevs)
import TauSigma.Statistics.Hadamard (hdevs)
import TauSigma.Statistics.Total (totdevs)
import TauSigma.Statistics.Theo1 (theo1devs, theoBRdevs, theoHdevs)

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
import TauSigma.Util.Vector


type Statistic = U.Vector (Time Double) -> [TauSigma Double]
type Noise m = Producer (TimeData Double) (Prob m) ()

main :: IO ()
main = defaultMain tests
  where tests = [ noiseTests
                , adevTests
                , mdevTests
                , hdevTests
                , totdevTests
                , theo1Tests
                , theoBRTests
                , theoHTests
                ]



noiseTests :: Benchmark
noiseTests = bgroup "noise" subgroups
  where
    sizes = [50, 500, 5000]
    subgroups = [ bgroup "wpm" (map benchWPM sizes)
                , bgroup "fpm" (map benchFPM sizes)
                , bgroup "wfm" (map benchWFM sizes)
                , bgroup "ffm" (map benchFFM sizes)
                , bgroup "rwfm" (map benchRWFM sizes)
                ]

benchWPM, benchFPM, benchWFM, benchFFM, benchRWFM :: Int -> Benchmark
benchWPM = benchNoise (whitePhase 1.0)
benchWFM = benchNoise (whiteFrequency 1.0 >-> toPhase)

benchRWFM = benchNoise (randomWalkFrequency 1.0 >-> toPhase)

benchFPM size = benchNoise (flickerPhase (octaves size) 1.0) size
benchFFM size =
  benchNoise (flickerFrequency (octaves size) 1.0 >-> toPhase) size

benchNoise :: Noise IO -> Int -> Benchmark
benchNoise noise size =
  bench (show size) $ nfIO (runWithCreate $ takeNoise size noise)

takeNoise :: PrimMonad m => Int -> Noise m -> Prob m (U.Vector Double)
takeNoise samples noise = takeVector samples (noise >-> P.map unTagged)



adevTests :: Benchmark
adevTests = bgroup "adev" (runStatistic statistic wfm sizes)
  where statistic = adevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [50, 500, 5000]

mdevTests :: Benchmark
mdevTests = bgroup "mdev" (runStatistic statistic wfm sizes)
  where statistic = mdevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [50, 500, 5000]

hdevTests :: Benchmark
hdevTests = bgroup "hdev" (runStatistic statistic wfm sizes)
  where statistic = hdevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [50, 500, 5000]

totdevTests :: Benchmark
totdevTests = bgroup "totdev" (runStatistic statistic wfm sizes)
  where statistic = totdevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [50, 500, 5000]

theo1Tests :: Benchmark
theo1Tests = bgroup "theo1" (runStatistic statistic wfm sizes)
  where statistic = theo1devs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [200, 400, 600]

theoBRTests :: Benchmark
theoBRTests = bgroup "theoBR" (runStatistic statistic wfm sizes) 
  where statistic = theoBRdevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [200, 400, 600]

theoHTests :: Benchmark
theoHTests = bgroup "theoH" (runStatistic statistic wfm sizes) 
  where statistic = theoHdevs 1
        wfm = whiteFrequency 1.0 >-> toPhase
        sizes = [200, 400, 600]


runStatistic
  :: Statistic
  -> Noise IO
  -> [Int]
  -> [Benchmark]
runStatistic statistic noise sizes =
  [ bgroup "seq" (map (runOne statistic) sizes)
  , bgroup "par" (map (runOne (parallelize statistic)) sizes)
  ]
  where runOne stat size = 
          env (runWithCreate $ takeNoise size noise) $ \input -> 
            bench (show size) $ nf stat input

runWithCreate :: Prob IO a -> IO a
runWithCreate ma = sample ma =<< create

parallelize :: Statistic -> Statistic
parallelize statistic = withStrategy (parBuffer 50 rdeepseq) . statistic
