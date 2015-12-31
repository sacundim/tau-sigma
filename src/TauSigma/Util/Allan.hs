
-- | Allan Variance and other related frequency stability statistics.
--
-- See: http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Util.Allan
       ( Tau0
       , integrate
       , differences
         
       , avar
       , avars
       , adev
       , adevs
       ) where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V


type Tau0 = Int


-- | All the functions in this module take phase error sequences as
-- input.  This function converts frequency error data to phase error
-- data.
integrate :: (Num a, Vector v a) => v a -> v a
integrate xs = V.scanl' (+) 0 xs

-- | Take the differences of consecutive elements of the vector.
-- Converts a sequence of time errors into a sequence of rate errors.
differences :: (Num a, Vector v a) => v a -> v a
differences xs | V.null xs = V.empty
differences xs = V.zipWith (+) (V.map negate xs) (V.tail xs)


avar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
avar tau0 m xs = sumsq / fromIntegral divisor
  where divisor = 2 * m^2 * tau0^2 * (V.length xs - 2*m)
        sumsq = sumGen (V.length xs - 2*m) step
          where step i = (xs!(i+2*m) - 2*(xs!(i+m)) + xs!i)^2

adev :: (Floating a, Vector v a) => Tau0-> Int -> v a -> a
adev tau0 m xs = sqrt (avar tau0 m xs)

avars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
avars tau0 xs = IntMap.fromList (map step [1..maxTaus])
  where step m = (m, avar tau0 m xs)
        maxTaus = (V.length xs - 1) `div` 2
                    
adevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
adevs tau0 xs = IntMap.map sqrt (avars tau0 xs)


-- Auxiliary functions

-- | This is equivalent to the composition of 'V.sum' and 'V.generate'.
sumGen :: Num a => Int -> (Int -> a) -> a
{-# INLINE sumGen #-}
sumGen n f = Stream.foldl' (+) 0 (Stream.generate n f)

