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
import qualified Data.Vector.Unboxed as U

import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


-- | Overlapped estimator of Allan variance at one sampling interval.
avar :: (Vector v Double) => OneTau v
{-# INLINABLE avar #-}
{-# SPECIALIZE avar :: OneTau U.Vector #-}
avar tau0 m xs = sumsq 0 (V.length xs - 2*m) term / divisor
  where divisor = 2 * m'^2 * tau0^2 * (len - 2*m')
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        term i  = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

-- | Overlapped estimator of Allan deviation at one sampling interval.
adev :: (Vector v Double) => OneTau v
{-# INLINABLE adev #-}
{-# SPECIALIZE adev :: OneTau U.Vector #-}
adev tau0 m xs = sqrt (avar tau0 m xs)

-- | Overlapped estimator of Allan variance at all sampling intervals.
avars :: (Vector v Double) => AllTau v
{-# INLINABLE avars #-}
{-# SPECIALIZE avars :: AllTau U.Vector #-}
avars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 2]
        go m = (fromIntegral m * tau0, avar tau0 m xs)

-- | Overlapped estimator of Allan deviation at all sampling intervals.
adevs :: (Vector v Double) => AllTau v
{-# INLINABLE adevs #-}
{-# SPECIALIZE adevs :: AllTau U.Vector #-}
adevs tau0 xs = over (traverse . _2) sqrt (avars tau0 xs)


-- | Estimator of Modified Allan variance at one sampling interval.
mvar :: (Vector v Double) => OneTau v
{-# INLINABLE mvar #-}
{-# SPECIALIZE mvar :: OneTau U.Vector #-}
{- FIXME: slow... -}
mvar tau0 m xs = outer / divisor
  where
    divisor = 2 * m'^4 * tau0^2 * (len - 3*m' + 1)
      where m' = fromIntegral m
            len = fromIntegral (V.length xs)
    outer = sumsq 0 (V.length xs - 3*m + 1) inner
      where inner j = summation "mvar" j (j + m) term
              where term i = xs!(i+2*m) - 2*(xs!(i+m)) + xs!i

mdev :: (Vector v Double) => OneTau v
{-# INLINABLE mdev #-}
{-# SPECIALIZE mdev :: OneTau U.Vector #-}
mdev tau0 m xs = sqrt (mvar tau0 m xs)

mvars :: (Vector v Double) => AllTau v
{-# INLINABLE mvars #-}
{-# SPECIALIZE mvars :: AllTau U.Vector #-}
mvars tau0 xs = map go taus
  where taus = [1 .. (V.length xs - 1) `div` 3]
        go m = (fromIntegral m * tau0, mvar tau0 m xs)
                    
mdevs :: (Vector v Double) => AllTau v
{-# INLINABLE mdevs #-}
{-# SPECIALIZE mdevs :: AllTau U.Vector #-}
mdevs tau0 xs = over (traverse . _2) sqrt (mvars tau0 xs)


tvar :: (Vector v Double) => OneTau v
{-# INLINABLE tvar #-}
{-# SPECIALIZE tvar :: OneTau U.Vector #-}
tvar tau0 m xs = mvar2tvar (tau0 * fromIntegral m) (mvar tau0 m xs)

tdev :: (Vector v Double) => OneTau v
{-# INLINABLE tdev #-}
{-# SPECIALIZE tdev :: OneTau U.Vector #-}
tdev tau0 m xs = sqrt (tvar tau0 m xs)

tvars :: (Vector v Double) => AllTau v
{-# INLINABLE tvars #-}
{-# SPECIALIZE tvars :: AllTau U.Vector #-}
tvars tau0 xs = map go (mvars tau0 xs)
  where go (tau, sigma) = (tau, mvar2tvar tau sigma)
                    
tdevs :: (Vector v Double) => AllTau v
{-# INLINABLE tdevs #-}
{-# SPECIALIZE tdevs :: AllTau U.Vector #-}
tdevs tau0 xs = over (traverse . _2) sqrt (tvars tau0 xs)

mvar2tvar :: Tau Double -> Sigma Double -> Sigma Double
{-# INLINE mvar2tvar #-}
mvar2tvar tau a = (tau^2 / 3) * a

