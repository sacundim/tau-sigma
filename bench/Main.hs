{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Criterion.Main

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Primitive.Class (MonadPrim)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Vector as V
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Vector

import System.Random.MWC.Monad (runWithCreate)

import TauSigma.Util.Allan
import TauSigma.Util.Pipes.Noise
import TauSigma.Util.Vector


main :: IO ()
main = defaultMain
  [ noiseTests
  , adevTests
  ]


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

benchNoise noise size = bench (show size) $ nfIO (makeUnboxedNoise noise size)

adevTests = bgroup "adev"
            [ bgroup "unboxed" (map (unboxed (white 1.0)) sizes)
            , bgroup "boxed" (map (boxed (white 1.0)) sizes)
            ]
  where sizes = [50, 500, 5000]
        unboxed source size =
          env (makeUnboxedNoise source size) $ \input -> 
              bench (show size) $ nf (adevs 1) input
        boxed source size =
          env (makeBoxedNoise source size) $ \input -> 
              bench (show size) $ nf (adevs 1) input


makeBoxedNoise
  :: (MonadPrim m) =>
     Producer Double (Rand m) () -> Int -> m (V.Vector Double)
makeBoxedNoise = makeGenericNoise


makeUnboxedNoise
  :: (MonadPrim m) =>
     Producer Double (Rand m) () -> Int -> m (V.Vector Double)
makeUnboxedNoise = makeGenericNoise


makeGenericNoise
  :: (MonadPrim m, Vector v Double) =>
     Producer Double (Rand m) () -> Int -> m (v Double)
makeGenericNoise source size =
  runWithCreate $ fmap G.fromList $ P.toListM $ (source >-> P.take size) 
