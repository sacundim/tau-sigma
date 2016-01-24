{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Theo1 stability statistic.  See:
--
-- * http://tf.nist.gov/timefreq/general/pdf/1990.pdf
-- * http://tf.nist.gov/general/pdf/2220.pdf
module TauSigma.Statistics.Theo1
       ( module TauSigma.Statistics.Types

       , isTheo1Tau
         
       , theo1var
       , theo1dev
       , theo1vars
       , theo1devs

       , theoBRvars
       , theoBRdevs

       , theoHvars
       , theoHdevs
       ) where

import Control.Lens (over, _2)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Allan (avar, avars)
import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


-- | Theo1 is only defined for certain sampling intervals.
isTheo1Tau :: Int -> Int -> Bool
isTheo1Tau m size = even m && 10 <= m && m <= size - 1

theo1Taus :: Int -> [Int]
theo1Taus size = filter (flip isTheo1Tau size) [1 .. 3 * (size `div` 4)]


theo1var
  :: (Fractional a, Vector v a) =>
     Tau0 a -> Int -> v (Time a) -> Maybe (Sigma a)
{-# INLINABLE theo1var #-}
theo1var tau0 m xs
  | m `isTheo1Tau` V.length xs =
      Just (unsafeTheo1var tau0 m xs)
  | otherwise = Nothing

theo1dev
  :: (Floating a, Vector v a) =>
     Tau0 a -> Int -> v (Time a) -> Maybe (Sigma a)
{-# INLINABLE theo1dev #-}
theo1dev tau0 m xs = fmap sqrt (theo1var tau0 m xs)

theo1vars :: (Fractional a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theo1vars #-}
theo1vars tau0 xs = map go (theo1Taus size)
  where size = V.length xs
        go m = (0.75 * fromIntegral m * tau0, unsafeTheo1var tau0 m xs)

theo1devs :: (Floating a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theo1devs #-}
theo1devs tau0 xs = over (traverse . _2) sqrt (theo1vars tau0 xs)


unsafeTheo1var
  :: (Fractional a, Vector v a) =>
     Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE unsafeTheo1var #-}
unsafeTheo1var tau0 m xs = outer / (0.75 * divisor)
  where divisor = (len - m') * (m' * tau0)^2
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        outer = summation 0 (V.length xs - m) middle
          where middle i = summation 0 (m `div` 2) inner
                  where inner d = term^2 / fromIntegral (halfM - d)
                          where halfM = m `div` 2
                                term = (xs!i - xs!(i - d + halfM))
                                     + (xs!(i+m) -xs!(i + d + halfM))

unsafeTheo1dev
  :: (Floating a, Vector v a) =>
     Tau0 a -> Int -> v (Time a) -> Sigma a
{-# INLINABLE unsafeTheo1dev #-}
unsafeTheo1dev tau0 m xs = sqrt (unsafeTheo1var tau0 m xs)


-- | Bias-reduced Theo1 variance.  
theoBRvars
  :: forall v a.
     (RealFrac a, Vector v a) =>
     Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theoBRvars #-}
theoBRvars tau0 xs = map go (theo1Taus (V.length xs))
  where
    go :: Int -> TauSigma a
    go m = (tau, sigma)
      where
        tau = 0.75 * fromIntegral m * tau0
        sigma = (ratio * theo1) / fromIntegral (n+1)
          where
            theo1 = unsafeTheo1var tau0 m xs
            n = floor (((0.1 * fromIntegral (V.length xs)) / 3) - 3)
            ratio = summation 0 (n+1) term
              where term i = avar tau0 (9 + 3*i) xs
                           / unsafeTheo1var tau0 (12 + 4*i) xs


theoBRdevs
  :: (Floating a, RealFrac a, Vector v a) =>
     Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theoBRdevs #-}
theoBRdevs tau0 xs = over (traverse . _2) sqrt (theoBRvars tau0 xs)


                           
theoHvars :: (RealFrac a, Vector v a) => Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theoHvars #-}
theoHvars tau0 xs = mergeOn fst allans theoBRs
  where k = fromIntegral (V.length xs `div` 10) * tau0
        allans = filter cutoff (avars tau0 xs)
          where cutoff (tau, _) = tau < k
        theoBRs = filter cutoff (theoBRvars tau0 xs)
          where cutoff (tau, _) = k <= tau

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ [] ys = ys
mergeOn _ xs [] = xs
mergeOn f xs'@(x:xs) ys'@(y:ys)
  | f x <= f y = x : mergeOn f xs ys'
  | otherwise = y : mergeOn f xs' ys

theoHdevs
  :: (RealFrac a, Floating a, Vector v a) =>
     Tau0 a -> v (Time a) -> [TauSigma a]
{-# INLINABLE theoHdevs #-}
theoHdevs tau0 xs = over (traverse . _2) sqrt (theoHvars tau0 xs)

