module Main (main) where

import Criterion.Main

import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Random (MonadRandom)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V

import Pipes
import Pipes.Prelude as P
import Pipes.Vector

import TauSigma.Util.Allan
import TauSigma.Util.Pipes.Noise
import TauSigma.Util.Vector


main :: IO ()
main = do
  white <- whiteNoise 1000
  defaultMain
       [ bgroup "adev"
         [ bench "wfm" $ nf (adevs 1) white
         ]
       ]


whiteNoise :: (MonadRandom m, PrimMonad m) => Int -> m (Vector Double)
whiteNoise size = readVector (white 1.0 >-> P.take size) 
    
