{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions for the Total deviation family of functions.  See:
--
-- * Howe, D.A. and C. A. Greenhall.  1997. \"Total Variance: a
--   Progress Report on a New Frequency Stability Characterization\."
--   Proc. 1997 PTTI Mtg., pp. 39-48 (Dec. 1997).  Web:
--   http://tf.nist.gov/general/pdf/1257.pdf
-- * Handbook of Frequency Stability Analysis
--
module TauSigma.Statistics.Total
       ( module TauSigma.Statistics.Types
       , (!*)
       , totvar
       , totdev
       , totvars
       , totdevs
       ) where

import Control.Lens (over, _2)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


-- | Extend a time error sequence by reflection around the ends.
(!*) :: Num a => Vector v a => v a -> Int -> a
{-# INLINE (!*) #-}
xs !* i
  | i < 0           = 2*(V.head xs) - xs!(-i)
  | i < V.length xs = xs!i
  | otherwise       = let j = V.length xs - (i `mod` V.length xs) - 2
                      in 2*(V.last xs) - xs!j

infixl 9 !*

-- | TOTVAR estimator at one sampling interval.
totvar :: (Fractional a, Vector v a) => Tau0 a -> Int -> v a -> a
{-# INLINABLE totvar #-}
totvar tau0 m xs = sumsq 0 (V.length xs - 1) term / divisor
  where divisor = 2 * m'^2 * tau0^2 * (len - 2)
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        term notI = xs!*(i-m) - 2*xs!*i + xs!*(i+m)
          where i = notI+1

-- | Overlapped estimator of Allan deviation at one sampling interval.
totdev :: (Floating a, Vector v a) => Tau0 a -> Int -> v a -> a
{-# INLINABLE totdev#-}
totdev tau0 m xs = sqrt (totvar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
totvars :: (RealFrac a, Vector v a) => Tau0 a -> v a -> [TauSigma a]
{-# INLINABLE totvars #-}
totvars tau0 xs = map go taus
  where taus = [1 .. V.length xs - 2]
        go m = (fromIntegral m * tau0, totvar tau0 m xs)

                    
-- | Overlapped estimator of Allan deviation at all sampling intervals.
totdevs :: (RealFloat a, Vector v a) => Tau0 a -> v a -> [TauSigma a]
{-# INLINABLE totdevs #-}
totdevs tau0 xs = over (traverse . _2) sqrt (totvars tau0 xs)


