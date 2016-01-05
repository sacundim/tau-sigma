
module TauSigma.Statistics.Util
       ( Tau0
       , integrate
       , differences
       , summation
       , sumsq
       , allTaus

       , Default
       ) where

import Data.Default (Default)

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import TauSigma.Util.DenseIntMap (IntMap)
import qualified TauSigma.Util.DenseIntMap as IntMap


type Tau0 = Int


-- | Converts frequency error data to phase error data.
integrate :: (Num a, Vector v a) => v a -> v a
{-# INLINE integrate #-}
integrate xs = V.scanl' (+) 0 xs

-- | Take the differences of consecutive elements of the vector.
-- Converts a sequence of time errors into a sequence of rate errors.
differences :: (Num a, Vector v a) => v a -> v a
{-# INLINE differences #-}
differences xs | V.null xs = V.empty
differences xs = V.zipWith (+) (V.map negate xs) (V.tail xs)


-- | Sum a series of 'Int'-indexed terms.  Inclusive start, exclusive end.
summation :: Num a => Int -> Int -> (Int -> a) -> a
{-# INLINE summation #-}
summation from to term
  | from > to = error "bad range in summation"
  | otherwise = go 0 from
  where go subtotal i
          | i < to    = go (subtotal + term i) (i+1)
          | otherwise = subtotal

-- | Sum the squares of a series of 'Int'-indexed terms.  Inclusive
-- start, exclusive end.
sumsq :: Num a => Int -> Int -> (Int -> a) -> a
{-# INLINE sumsq #-}
sumsq from to term
  | from > to = error "bad range in summation"
  | otherwise = go 0 from
  where go subtotal i
          | i < to    = go (subtotal + (term i)^2) (i+1)
          | otherwise = subtotal


-- | Auxiliary function to compute one statistic at all given taus
allTaus :: (Vector v a, Default a) =>
           [Tau0] -> (Tau0 -> v a -> a) -> v a -> IntMap a
{-# INLINE allTaus #-}
allTaus taus statistic xs = IntMap.fromList (map step taus)
  where step m = (m, statistic m xs)
