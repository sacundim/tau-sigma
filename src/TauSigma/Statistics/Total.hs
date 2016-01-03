
-- | Utility functions for the Total deviation family of functions.  See:
--
-- * Howe, D.A. and C. A. Greenhall.  1997. \"Total Variance: a
--   Progress Report on a New Frequency Stability Characterization\."
--   Proc. 1997 PTTI Mtg., pp. 39-48 (Dec. 1997).  Web:
--   http://tf.nist.gov/general/pdf/1257.pdf
-- * Handbook of Frequency Stability Analysis
--
module TauSigma.Statistics.Total
       ( Tau0
       , (!*)
       , totvar
       , totdev
       , totvars
       , totdevs
       ) where

import Data.IntMap.Lazy (IntMap)
import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Util


-- | Extend a time error sequence by reflection around the ends.
(!*) :: Num a => Vector v a => v a -> Int -> a
{-# INLINE (!*) #-}
xs !* i
  | i < 0           = 2*(V.head xs) - xs!(-i)
  | i < V.length xs = xs!i
  | otherwise       = let j = V.length xs - (i `mod` V.length xs) - 2
                      in 2*(V.last xs) - xs!j

{-
xs !* i
  | i < 0 = 2*xs!0 - xs!(-i)
  | V.length xs <= i = 2*(V.last xs) - xs!(V.length xs - i)
  | otherwise = xs!i
-}
infixl 9 !*

-- | TOTVAR estimator at one sampling interval.
totvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
totvar tau0 m xs = sumsq 0 (V.length xs - 1) term / fromIntegral divisor
  where divisor = 2 * m^2 * tau0^2 * (V.length xs - 2)
        term notI = xs!*(i-m) - 2*xs!*i + xs!*(i+m)
          where i = notI+1

-- | Overlapped estimator of Allan deviation at one sampling interval.
totdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
totdev tau0 m xs = sqrt (totvar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
totvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
totvars tau0 xs = allTaus [1..maxTaus] (totvar tau0) xs
  where maxTaus = V.length xs - 2
                    
-- | Overlapped estimator of Allan deviation at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want
-- right away and discard the map!
totdevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
totdevs tau0 xs = allTaus [1..maxTaus] (totdev tau0) xs
  where maxTaus = V.length xs - 1


