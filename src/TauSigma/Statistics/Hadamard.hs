{-# LANGUAGE FlexibleContexts #-}

-- | Hadamard variance and deviation estimators.  See:
--
-- * http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Statistics.Hadamard
       ( module TauSigma.Statistics.Types
       , hvar
       , hvars
       , hdev
       , hdevs
       ) where

import Control.Lens (over, _2)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


-- | Overlapped estimator of Hadamard variance at one sampling interval.
hvar :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE hvar #-}
hvar tau0 m xs = sumsq 0 (V.length xs - 3*m) term / divisor
  where divisor = 6 * m'^2 * tau0^2 * (len - 3*m')
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        term i = xs!(i+3*m) - 3*xs!(i+2*m) + 3*xs!(i+m) - xs!i

-- | Overlapped estimator of Hadamard deviation at one sampling interval.
hdev :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE hdev #-}
hdev tau0 m xs = sqrt (hvar tau0 m xs)

-- | Overlapped estimator of Hadamard variance at all sampling intervals.
-- Note that this returns a lazy list whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the rest!
hvars :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE hvars #-}
hvars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 3]
        go m = (fromIntegral m * tau0, hvar tau0 m xs)
                    
-- | Overlapped estimator of Hadamard deviation at all sampling intervals.
-- Note that this returns a lazy list whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the rest!
hdevs :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE hdevs #-}
hdevs tau0 xs = over (traverse . _2) sqrt (hvars tau0 xs)
