{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Criterion.Main

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Random (MonadRandom)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Vector

import TauSigma.Util.Allan
import TauSigma.Util.Pipes.Noise
import TauSigma.Util.Vector


main :: IO ()
main = defaultMain
  [ noiseTests
  , adevTests
  ]


makeNoise
  :: (MonadRandom m, PrimMonad m) =>
     Producer Double m () -> Int -> m (U.Vector Double)
makeNoise source size = readVector (source >-> P.take size) 

makeBoxedNoise
  :: (MonadRandom m, PrimMonad m) =>
     Producer Double m () -> Int -> m (V.Vector Double)
makeBoxedNoise source size = readVector (source >-> P.take size) 


noiseTests = bgroup "noise" subgroups
  where
    sizes = [50, 500, 5000]
    subgroups = [ bgroup "white"   (map benchWhite sizes)
                , bgroup "brown"   (map benchBrown sizes)
                , bgroup "flicker" (map benchFlicker sizes)
                ]

benchWhite = benchNoise (white 1.0)
benchBrown = benchNoise (brown 1.0)
benchFlicker size = benchNoise (flicker octaves 1.0) size
  where octaves = floor (logBase 2 (fromIntegral size)) + 1

benchNoise noise size = bench (show size) $ nfIO (makeNoise noise size)

adevTests = bgroup "adev"
            [ bgroup "unboxed" (map (unboxed (white 1.0)) sizes)
            , bgroup "boxed" (map (boxed (white 1.0)) sizes)
            ]
  where sizes = [50, 500, 5000]
        unboxed source size =
          env (makeNoise source size) $ \input -> 
              bench (show size) $ nf (adevs 1) input
        boxed source size =
          env (makeBoxedNoise source size) $ \input -> 
              bench (show size) $ nf (adevs 1) input
