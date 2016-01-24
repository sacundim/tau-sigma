{-# LANGUAGE FlexibleContexts #-}

-- | Allan variance and deviation estimators.  See:
--
-- * http://tf.nist.gov/general/pdf/2220.pdf
--
module TauSigma.Statistics.Allan
       ( module TauSigma.Statistics.Types
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

import Control.Lens (over, _2)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


-- | Overlapped estimator of Allan variance at one sampling interval.
avar :: (Fractional a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE avar #-}
avar tau0 m xs = sumsq 0 (V.length xs - 2*m) term / divisor
  where divisor = 2 * m'^2 * tau0^2 * (len - 2*m')
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        term i  = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

-- | Overlapped estimator of Allan deviation at one sampling interval.
adev :: (Floating a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE adev #-}
adev tau0 m xs = sqrt (avar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
avars :: (RealFrac a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE avars #-}
avars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 2]
        go m = (fromIntegral m * tau0, avar tau0 m xs)

-- | Overlapped estimator of Allan deviation at all sampling intervals.
adevs :: (RealFloat a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE adevs #-}
adevs tau0 xs = over (traverse . _2) sqrt (avars tau0 xs)


-- | Estimator of Modified Allan variance at one sampling interval.
mvar :: (Fractional a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE mvar #-}
{- FIXME: slow... -}
mvar tau0 m xs = outer / divisor
  where
    divisor = 2 * m'^4 * tau0^2 * (len - 3*m' + 1)
      where m' = fromIntegral m
            len = fromIntegral (V.length xs)
    outer = sumsq 0 (V.length xs - 3*m + 1) inner
      where inner j = summation j (j + m) term
              where term i = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

mdev :: (Floating a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE mdev #-}
mdev tau0 m xs = sqrt (mvar tau0 m xs)

mvars :: (RealFrac a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE mvars #-}
mvars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 3]
        go m = (fromIntegral m * tau0, mvar tau0 m xs)
                    
mdevs :: (RealFloat a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE mdevs #-}
mdevs tau0 xs = over (traverse . _2) sqrt (mvars tau0 xs)


tvar :: (Fractional a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE tvar #-}
tvar tau0 m xs = fst $ mvar2tvar (tau0 * fromIntegral m, mvar tau0 m xs)

tdev :: (Floating a, Vector v a) => Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE tdev #-}
tdev tau0 m xs = sqrt (tvar tau0 m xs)

tvars :: (RealFrac a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE tvars #-}
tvars tau0 xs = map mvar2tvar (mvars tau0 xs)
                    
tdevs :: (RealFloat a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE tdevs #-}
tdevs tau0 xs = over (traverse . _2) sqrt (tvars tau0 xs)

mvar2tvar :: Fractional a => TauSigma a -> TauSigma a
{-# INLINE mvar2tvar #-}
mvar2tvar (tau, a) = (tau, (tau^2 / 3) * a)

