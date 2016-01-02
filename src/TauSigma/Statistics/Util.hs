
module TauSigma.Statistics.Util
       ( Tau0
       , integrate
       , differences
       , sumGen
       ) where

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Fusion.Stream as Stream


type Tau0 = Int

-- | All the functions in this module take phase error sequences as
-- input.  This function converts frequency error data to phase error
-- data.
integrate :: (Num a, Vector v a) => v a -> v a
{-# INLINE integrate #-}
integrate xs = V.scanl' (+) 0 xs

-- | Take the differences of consecutive elements of the vector.
-- Converts a sequence of time errors into a sequence of rate errors.
differences :: (Num a, Vector v a) => v a -> v a
{-# INLINE differences #-}
differences xs | V.null xs = V.empty
differences xs = V.zipWith (+) (V.map negate xs) (V.tail xs)

-- | This is equivalent to the composition of 'V.sum' and 'V.generate'.
sumGen :: Num a => Int -> (Int -> a) -> a
{-# INLINE sumGen #-}
sumGen n f = Stream.foldl' (+) 0 (Stream.generate n f)

