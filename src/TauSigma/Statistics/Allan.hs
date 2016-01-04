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

       , mvar
       , mvars
       , mdev
       , mdevs

       , tvar
       , tvars
       , tdev
       , tdevs
       ) where

import Data.IntMap.Lazy (IntMap)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Util


-- | Overlapped estimator of Allan variance at one sampling interval.
avar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE avar #-}
avar tau0 m xs = sumsq 0 (V.length xs - 2*m) term / fromIntegral divisor
  where divisor :: Integer
        divisor = 2 * m'^2 * tau0'^2 * (len - 2*m')
          where m' = fromIntegral m
                tau0' = fromIntegral tau0
                len = fromIntegral (V.length xs)
        term i  = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

-- | Overlapped estimator of Allan deviation at one sampling interval.
adev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE adev #-}
adev tau0 m xs = sqrt (avar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want right
-- away and discard the map!
avars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE avars #-}
avars tau0 xs = allTaus [1..maxTaus] (avar tau0) xs
  where maxTaus = (V.length xs - 1) `div` 2
                    
-- | Overlapped estimator of Allan deviation at all sampling intervals.
-- Note that this returns a lazy 'IntMap' whose thunks hold on to the
-- input vector.  You're going to want to force the ones you want
-- right away and discard the map!
adevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE adevs #-}
adevs tau0 xs = allTaus [1..maxTaus] (adev tau0) xs
  where maxTaus = (V.length xs - 1) `div` 2


-- | Estimator of Modified Allan variance at one sampling interval.
mvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE mvar #-}
{- FIXME: slow... -}
mvar tau0 m xs = outer / fromIntegral divisor
  where
    -- Whoah, I was using just 'Int' here and I was getting overflows
    -- into negative values.
    divisor :: Integer
    divisor = 2 * m'^4 * tau0'^2 * (len - 3*m' + 1)
      where tau0' = fromIntegral tau0
            m' = fromIntegral m
            len = fromIntegral (V.length xs)
    outer = sumsq 0 (V.length xs - 3*m + 1) inner
      where inner j = summation j (j + m) term
              where term i = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

mdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE mdev #-}
mdev tau0 m xs = sqrt (mvar tau0 m xs)

mvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE mvars #-}
mvars tau0 xs = allTaus [1..maxTaus] (mvar tau0) xs
  where maxTaus = (V.length xs - 1) `div` 3
                    
mdevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE mdevs #-}
mdevs tau0 xs = allTaus [1..maxTaus] (mdev tau0) xs
  where maxTaus = (V.length xs - 1) `div` 3



tvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE tvar #-}
tvar tau0 m xs = (fromIntegral (m*tau0)^2 / 3) * mvar tau0 m xs

tdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE tdev #-}
tdev tau0 m xs = sqrt (tvar tau0 m xs)

tvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE tvars #-}
tvars tau0 xs = allTaus [1..maxTaus] (tvar tau0) xs
  where maxTaus = (V.length xs - 1) `div` 3
                    
tdevs :: (RealFloat a, Vector v a) => Tau0 -> v a -> IntMap a
{-# INLINABLE tdevs #-}
tdevs tau0 xs = allTaus [1..maxTaus] (tdev tau0) xs
  where maxTaus = (V.length xs - 1) `div` 3
