{-# LANGUAGE FlexibleContexts #-}

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

import Data.Default (Default)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Util
import TauSigma.Util.DenseIntMap (DenseIntMap, Entry(..))
import qualified TauSigma.Util.DenseIntMap as IntMap


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
avars :: (RealFrac a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE avars #-}
avars tau0 xs = IntMap.fromEntries (V.generate (taus + 1) go)
  where taus = V.length xs - 1 `div` 2
        go 0 = Entry False 0.0
        go m = Entry True (avar tau0 m xs)

-- | Overlapped estimator of Allan deviation at all sampling intervals.
adevs :: (RealFloat a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE adevs #-}
adevs tau0 xs = IntMap.map sqrt (avars tau0 xs)


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

mvars :: (RealFrac a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE mvars #-}
mvars tau0 xs = IntMap.fromEntries (V.generate (taus + 1) go)
  where taus = V.length xs - 1 `div` 3
        go 0 = Entry False 0.0
        go m = Entry True (mvar tau0 m xs)
                    
mdevs :: (RealFloat a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE mdevs #-}
mdevs tau0 xs = IntMap.map sqrt (mvars tau0 xs)


tvar :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE tvar #-}
tvar tau0 m xs = mvar2tvar tau0 m (mvar tau0 m xs)

tdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE tdev #-}
tdev tau0 m xs = sqrt (tvar tau0 m xs)

tvars :: (RealFrac a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE tvars #-}
tvars tau0 xs = IntMap.mapWithKey (mvar2tvar tau0) (avars tau0 xs)
                    
tdevs :: (RealFloat a, Default a, Vector v a, Vector v (Entry a)) =>
         Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE tdevs #-}
tdevs tau0 xs = IntMap.map sqrt (tvars tau0 xs)

mvar2tvar :: Fractional a => Tau0 -> Int -> a -> a
{-# INLINE mvar2tvar #-}
mvar2tvar tau0 m a = (fromIntegral (m*tau0)^2 / 3) * a

