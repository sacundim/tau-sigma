{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Theo1 stability statistic.  See:
--
-- * http://tf.nist.gov/timefreq/general/pdf/1990.pdf
-- * http://tf.nist.gov/general/pdf/2220.pdf
-- * http://tf.nist.gov/general/pdf/2262.pdf
module TauSigma.Statistics.Theo1
       ( module TauSigma.Statistics.Types

       , theo1Tau
       , isTheo1Step
         
       , theo1var
       , theo1dev
       , theo1vars
       , theo1devs

       , theoBRvars
       , theoBRdevs

       , theoHvars
       , theoHdevs
       ) where

import Control.Lens (over, _2, _Just)

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

import TauSigma.Statistics.Allan (avar, avars)
import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


theo1Tau :: Tau0 Double -> Int -> Maybe (Tau Double)
theo1Tau tau0 m
  | even m && 10 <= m = Just (unsafeTheo1Tau tau0 m)
  | otherwise         = Nothing

unsafeTheo1Tau :: Tau0 Double -> Int -> Tau Double
unsafeTheo1Tau tau0 m = 0.75 * fromIntegral m * tau0



-- | Theo1 is only defined for certain sampling intervals.
isTheo1Step :: Int -> Int -> Bool
isTheo1Step m size = even m && 10 <= m && m <= size - 1

theo1Steps :: Int -> [Int]
theo1Steps size = filter (flip isTheo1Step size) [1 .. 3 * (size `div` 4)]


theo1var
  :: (Vector v Double) =>
     Tau0 Double -> Int -> v (Time Double) -> Maybe (TauSigma Double)
{-# INLINABLE theo1var #-}
theo1var tau0 m xs
  | m `isTheo1Step` V.length xs =
      Just (unsafeTheo1Tau tau0 m, unsafeTheo1var tau0 m xs)
  | otherwise = Nothing

theo1dev
  :: (Vector v Double) =>
     Tau0 Double -> Int -> v (Time Double) -> Maybe (TauSigma Double)
{-# INLINABLE theo1dev #-}
theo1dev tau0 m xs = over (_Just . _2) sqrt (theo1var tau0 m xs)

theo1vars :: (Vector v Double) => AllTau v
{-# INLINABLE theo1vars #-}
{-# SPECIALIZE theo1vars :: AllTau U.Vector #-}
theo1vars tau0 xs = map go (theo1Steps size)
  where size = V.length xs
        go m = (unsafeTheo1Tau tau0 m, unsafeTheo1var tau0 m xs)

theo1devs :: (Vector v Double) => AllTau v
{-# INLINABLE theo1devs #-}
{-# SPECIALIZE theo1devs :: AllTau U.Vector #-}
theo1devs tau0 xs = over (traverse . _2) sqrt (theo1vars tau0 xs)


unsafeTheo1var :: (Vector v Double) => OneTau v
{-# INLINABLE unsafeTheo1var #-}
{-# SPECIALIZE unsafeTheo1var :: OneTau U.Vector #-}
unsafeTheo1var tau0 m xs = outer / (0.75 * divisor)
  where divisor = (len - m') * (m' * tau0)^2
          where m' = fromIntegral m
                len = fromIntegral (V.length xs)
        outer = summation "outer" 0 (V.length xs - m) middle
          where middle i = summation "middle" 0 (m `div` 2) inner
                  where inner d = term^2 / fromIntegral (halfM - d)
                          where halfM = m `div` 2
                                term = (xs!i - xs!(i - d + halfM))
                                     + (xs!(i+m) -xs!(i + d + halfM))

unsafeTheo1dev :: (Vector v Double) => OneTau v
{-# INLINABLE unsafeTheo1dev #-}
{-# SPECIALIZE unsafeTheo1dev :: OneTau U.Vector #-}
unsafeTheo1dev tau0 m xs = sqrt (unsafeTheo1var tau0 m xs)


-- | Bias-reduced Theo1 variance.  
theoBRvars :: (Vector v Double) => AllTau v
{-# INLINABLE theoBRvars #-}
{-# SPECIALIZE theoBRvars :: AllTau U.Vector #-}
theoBRvars tau0 xs
  | n > 0 = map go (theo1Steps (V.length xs))
  | otherwise = []
  where
    n :: Int
    (n, bias) = theoBRBias tau0 xs

    go :: Int -> TauSigma Double
    go m = (tau, sigma)
      where
        tau = 0.75 * fromIntegral m * tau0
        sigma = (bias * theo1) / fromIntegral (n+1)
          where theo1 = unsafeTheo1var tau0 m xs

theoBRBias
  :: (Vector v Double) =>
     Tau0 Double
  -> v Double
  -> (Int, Double)
{-# INLINE theoBRBias #-}
theoBRBias tau0 xs = (n, summation "bias" 0 (n+1) term)
  where n :: Int
        n = floor (((0.1 * fromIntegral (V.length xs)) / 3) - 3)

        term :: Int -> Double
        term i = avar tau0 (9 + 3*i) xs
               / unsafeTheo1var tau0 (12 + 4*i) xs


theoBRdevs :: (Vector v Double) => AllTau v
{-# INLINABLE theoBRdevs #-}
{-# SPECIALIZE theoBRdevs :: AllTau U.Vector #-}
theoBRdevs tau0 xs = over (traverse . _2) sqrt (theoBRvars tau0 xs)

                           
theoHvars :: (Vector v Double) => AllTau v
{-# INLINABLE theoHvars #-}
{-# SPECIALIZE theoHvars :: AllTau U.Vector #-}
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

theoHdevs :: (Vector v Double) => AllTau v
{-# INLINABLE theoHdevs #-}
{-# SPECIALIZE theoHdevs :: AllTau U.Vector #-}
theoHdevs tau0 xs = over (traverse . _2) sqrt (theoHvars tau0 xs)

