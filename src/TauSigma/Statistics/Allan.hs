-- | Allan variance and deviation estimators.  See:
--
-- * http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Statistics.Allan
       ( Tau0
       , avar
       , avars
       , adev
       , adevs
       ) where

import Data.IntMap.Lazy (IntMap)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Util


-- | Overlapped estimator of Allan variance at one sampling interval.
avar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
avar tau0 m xs = sumsq / fromIntegral divisor
  where divisor = 2 * m^2 * tau0^2 * (V.length xs - 2*m)
        sumsq = sumGen (V.length xs - 2*m) step
          where step i = (xs!(i+2*m) - 2*(xs!(i+m)) + xs!i)^2

-- | Overlapped estimator of Allan deviation at one sampling interval.
adev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
adev tau0 m xs = sqrt (avar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
avars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
avars tau0 xs = allTaus [1..maxTaus] (avar tau0) xs
  where maxTaus = (V.length xs - 1) `div` 2
                    
-- | Overlapped estimator of Allan deviation at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want
-- right away and discard the map!
adevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
adevs tau0 xs = allTaus [1..maxTaus] (adev tau0) xs
  where maxTaus = (V.length xs - 1) `div` 2


