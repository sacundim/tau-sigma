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
avar :: (Vector v Double) =>
        Tau0 Double
     -> Int
     -> v (Time Double)
     -> Sigma Double
{-# INLINABLE avar #-}
avar tau0 m xs = sumsq 0 (V.length xs - 2*m) term / divisor
  where divisor = 2 * m'^2 * tau0^2 * (len - 2*m')
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        term i  = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

-- | Overlapped estimator of Allan deviation at one sampling interval.
adev :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE adev #-}
adev tau0 m xs = sqrt (avar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
avars :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE avars #-}
avars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 2]
        go m = (fromIntegral m * tau0, avar tau0 m xs)

-- | Overlapped estimator of Allan deviation at all sampling intervals.
adevs :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE adevs #-}
adevs tau0 xs = over (traverse . _2) sqrt (avars tau0 xs)


-- | Estimator of Modified Allan variance at one sampling interval.
mvar :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE mvar #-}
{- FIXME: slow... -}
mvar tau0 m xs = outer / divisor
  where
    divisor = 2 * m'^4 * tau0^2 * (len - 3*m' + 1)
      where m' = fromIntegral m
            len = fromIntegral (V.length xs)
    outer = sumsq 0 (V.length xs - 3*m + 1) inner
      where inner j = summation "mvar" j (j + m) term
              where term i = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

mdev :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE mdev #-}
mdev tau0 m xs = sqrt (mvar tau0 m xs)

mvars :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE mvars #-}
mvars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 3]
        go m = (fromIntegral m * tau0, mvar tau0 m xs)
                    
mdevs :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE mdevs #-}
mdevs tau0 xs = over (traverse . _2) sqrt (mvars tau0 xs)


tvar :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE tvar #-}
tvar tau0 m xs = mvar2tvar (tau0 * fromIntegral m) (mvar tau0 m xs)

tdev :: (Vector v Double) =>
        Tau0 Double -> Int -> v (Time Double) -> Sigma Double
{-# INLINABLE tdev #-}
tdev tau0 m xs = sqrt (tvar tau0 m xs)

tvars :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE tvars #-}
tvars tau0 xs = map go (mvars tau0 xs)
  where go (tau, sigma) = (tau, mvar2tvar tau sigma)
                    
tdevs :: (Vector v Double) =>
         Tau0 Double -> v (Time Double) -> [TauSigma Double]
{-# INLINABLE tdevs #-}
tdevs tau0 xs = over (traverse . _2) sqrt (tvars tau0 xs)

mvar2tvar :: Tau Double -> Sigma Double -> Sigma Double
{-# INLINE mvar2tvar #-}
mvar2tvar tau a = (tau^2 / 3) * a

