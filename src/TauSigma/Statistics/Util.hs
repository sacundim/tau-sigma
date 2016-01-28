{-# LANGUAGE FlexibleContexts #-}

module TauSigma.Statistics.Util
       ( integrate
       , differences
       , summation
       , sumsq
       ) where

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import Numeric.Sum (KBNSum, kbn, add, zero)

import Text.Printf


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
summation
  :: String           -- ^ A name to print in errors (for troubleshooting).
  -> Int              -- ^ Starting index (inclusive)
  -> Int              -- ^ Ending index (exclusive)
  -> (Int -> Double)  -- ^ Term of summation
  -> Double
{-# INLINE summation #-}
summation name from to term
  | from > to =
      error (printf "%s: bad range in summation: %d to %d" name from to)
  | otherwise = kbn (go zero from)
  where go :: KBNSum -> Int -> KBNSum
        go subtotal i
          | i < to    = go (subtotal `add` term i) (i+1)
          | otherwise = subtotal

-- | Sum the squares of a series of 'Int'-indexed terms.  Inclusive
-- start, exclusive end.
sumsq :: Int -> Int -> (Int -> Double) -> Double
{-# INLINE sumsq #-}
sumsq from to term
  | from > to = error (printf "bad range in sumsq: %d to %d" from to)
  | otherwise = kbn (go zero from)
  where go :: KBNSum -> Int -> KBNSum
        go subtotal i
          | i < to    = go (subtotal `add` ((term i)^2)) (i+1)
          | otherwise = subtotal

