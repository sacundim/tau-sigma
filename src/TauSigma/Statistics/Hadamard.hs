-- | Hadamard variance and deviation estimators.
--
-- See: http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Statistics.Hadamard
       ( Tau0
       , hvar
       , hvars
       , hdev
       , hdevs
       ) where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Util


-- | Overlapped estimator of Hadamard variance at one sampling interval.
hvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
hvar tau0 m xs = sumsq / fromIntegral divisor
  where divisor = 6 * (V.length xs - 3*m) * tau0^2
        sumsq = sumGen (V.length xs - 3*m) term
          where term i = (xs!(i+3*m) - 3*xs!(i+2*m) + 3*xs!(i+m) - xs!i)^2

-- | Overlapped estimator of Hadamard deviation at one sampling interval.
hdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
hdev tau0 m xs = sqrt (hvar tau0 m xs)

-- | Overlapped estimator of Hadamard variance at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
hvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
hvars tau0 xs = IntMap.fromList (map step [1..maxTaus])
  where step m = (m, hvar tau0 m xs)
        maxTaus = (V.length xs - 1) `div` 3
                    
-- | Overlapped estimator of Hadamard deviation at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
hdevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
hdevs tau0 xs = IntMap.map sqrt (hvars tau0 xs)
