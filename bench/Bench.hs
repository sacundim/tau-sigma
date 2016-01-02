{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Criterion.Main

import Control.Monad.Primitive (PrimMonad)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Pipes

import System.Random.MWC.Monad (Rand, runWithCreate)

import TauSigma.Statistics.Allan
import TauSigma.Statistics.Theo1
import TauSigma.Util.Pipes.Noise
import TauSigma.Util.Vector


main :: IO ()
main = defaultMain
  [ noiseTests
  , adevTests
  , theoBRTests
  ]

noiseTests :: Benchmark
noiseTests = bgroup "noise" subgroups
  where
    sizes = [50, 500, 5000]
    subgroups = [ bgroup "white"   (map benchWhite sizes)
                , bgroup "brown"   (map benchBrown sizes)
                , bgroup "flicker" (map benchFlicker sizes)
                ]

benchWhite, benchBrown, benchFlicker :: Int -> Benchmark
benchWhite = benchNoise (white 1.0)
benchBrown = benchNoise (brown 1.0)
benchFlicker size = benchNoise (flicker (octaves size) 1.0) size

benchNoise :: Producer Double (Rand IO) () -> Int -> Benchmark
benchNoise noise size =
  bench (show size) $ nfIO (runWithCreate $ makeUnboxedNoise size noise)

adevTests :: Benchmark
adevTests = bgroup "adev"
            [ bgroup "unboxed" (map (unboxed (white 1.0)) sizes)
            , bgroup "boxed" (map (boxed (white 1.0)) sizes)
            ]
  where sizes = [50, 500, 5000]
        unboxed source size =
          env (runWithCreate $ makeUnboxedNoise size source) $ \input -> 
              bench (show size) $ nf (adevs 1) input
        boxed source size =
          env (runWithCreate $ makeBoxedNoise size source) $ \input -> 
              bench (show size) $ nf (adevs 1) input


theoBRTests :: Benchmark
theoBRTests = bgroup "theoBR"
            [ bgroup "unboxed" (map (unboxed (white 1.0)) sizes)
            ]
  where sizes = [200, 400, 600]
        unboxed source size =
          env (runWithCreate $ makeUnboxedNoise size source) $ \input -> 
              bench (show size) $ nf (theoBRdevs 1) input


-- NOTE: This always uses the same random seed.
makeBoxedNoise
  :: PrimMonad m =>
     Int -> Producer Double (Rand m) () -> Rand m (V.Vector Double)
makeBoxedNoise n = takeVector n


-- NOTE: This always uses the same random seed.
makeUnboxedNoise
  :: PrimMonad m =>
     Int -> Producer Double (Rand m) () -> Rand m (V.Vector Double)
makeUnboxedNoise n = takeVector n

