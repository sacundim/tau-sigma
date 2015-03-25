{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- | Allan Variance and other related frequency stability statistics.
--
-- See: http://tf.nist.gov/general/pdf/2220.pdf
--
module Statistics.Allan
       ( integrate
       , avar
       , adev
       , tauSigma
       , maxTaus
       ) where

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V


-- | All the functions in this module take phase error sequences as
-- input.  This function converts frequency error data to phase error
-- data.
integrate :: (Num a, Vector v a) => v a -> v a
integrate xs = V.scanl' (+) 0 xs

avar :: forall v a. (Fractional a, Vector v a) => Int -> Int -> v a -> a
avar tau0 n xs = sumsq / fromIntegral divisor
  where divisor = 2 * n^2 * tau0^2 * (V.length xs - 2*n)
        sumsq = V.sum (V.generate (V.length xs - 2*n) step :: v a)
          where step i = (xs!(i+2*n) - 2*(xs!(i+n)) + xs!i)^2

adev :: (Floating a, Vector v a) => Int -> Int -> v a -> a
adev tau0 n xs = sqrt (avar tau0 n xs)

-- | Generate a tau-sigma table for the number of @ns@ specified.  The
-- nth position of the result corresponds to the value of the
-- statistic at a sampling interval of @(n+1)*tau0@.
--
-- Example use:
--
-- > tauSigma (adev 86400) 7 timeErrorData
tauSigma :: (Vector v a, Vector v Int) =>
            (Int -> v a -> a) -> Int -> v a -> v a
tauSigma estimator ns xs = V.map step (V.enumFromN 1 howManyTaus)
  where step n = estimator n xs 
        howManyTaus = min ns (maxTaus xs)

maxTaus :: Vector v a => v a -> Int
maxTaus xs = (V.length xs - 1) `div` 2
