{-# LANGUAGE FlexibleContexts #-}

-- | Hadamard variance and deviation estimators.  See:
--
-- * http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Statistics.Hadamard
       ( Tau0
       , hvar
       , hvars
       , hdev
       , hdevs
       ) where

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import TauSigma.Statistics.Util


-- | Overlapped estimator of Hadamard variance at one sampling interval.
hvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE hvar #-}
hvar tau0 m xs = sumsq 0 (V.length xs - 3*m) term / fromIntegral divisor
  where divisor :: Integer
        divisor = 6 * m'^2 * tau0'^2 * (len - 3*m')
          where m' = fromIntegral m
                tau0' = fromIntegral tau0
                len = fromIntegral (V.length xs)
        term i = xs!(i+3*m) - 3*xs!(i+2*m) + 3*xs!(i+m) - xs!i

-- | Overlapped estimator of Hadamard deviation at one sampling interval.
hdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE hdev #-}
hdev tau0 m xs = sqrt (hvar tau0 m xs)

-- | Overlapped estimator of Hadamard variance at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
hvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE hvars #-}
hvars tau0 xs = IntMap.fromList (map go taus)
  where taus = [1 .. (V.length xs - 1) `div` 3]
        go m = (m, hvar tau0 m xs)
                    
-- | Overlapped estimator of Hadamard deviation at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
hdevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE hdevs #-}
hdevs tau0 xs = IntMap.map sqrt (hvars tau0 xs)
