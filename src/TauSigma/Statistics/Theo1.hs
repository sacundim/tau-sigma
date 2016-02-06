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

import Math.NumberTheory.Powers (integerCubeRoot)
import Numeric.Sum (kbn, sumVector)

import TauSigma.Statistics.Allan (avar, avars)
import TauSigma.Statistics.Types
import TauSigma.Statistics.Util


--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- Utility functions
-- 

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


--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- Theo1
-- 

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


--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- TheoBR
-- 

-- | Bias-reduced Theo1 variance.  
theoBRvars :: (Vector v Double) => AllTau v
{-# INLINABLE theoBRvars #-}
{-# SPECIALIZE theoBRvars :: AllTau U.Vector #-}
theoBRvars tau0 xs
  | n > 0 = map go (theo1Steps (V.length xs))
  | otherwise = []
  where
    n :: Int
    n = theoBR_N (V.length xs)

    bias :: Double
    bias = theoBRBias tau0 xs

    go :: Int -> TauSigma Double
    go m = (tau, sigma)
      where
        tau = 0.75 * fromIntegral m * tau0
        sigma = (bias * theo1) / fromIntegral (n+1)
          where theo1 = unsafeTheo1var tau0 m xs

theoBRdevs :: (Vector v Double) => AllTau v
{-# INLINABLE theoBRdevs #-}
{-# SPECIALIZE theoBRdevs :: AllTau U.Vector #-}
theoBRdevs tau0 xs = over (traverse . _2) sqrt (theoBRvars tau0 xs)


-- | The "n" value used in various places of the TheoBR calculation,
-- which is a function of the size of the input data set.
theoBR_N :: Int -> Int
{-# INLINE theoBR_N #-}
theoBR_N size = floor (((0.1 * fromIntegral size) / 3) - 3)


-- | Smart estimate of TheoBR bias, using 'fastTheoBRBias' with an
-- automatically selected value for the Bias Average.
theoBRBias
  :: (Vector v Double) =>
     Tau0 Double  -- ^ Sampling interval
  -> v Double     -- ^ Time error samples
  -> Double       -- ^ The @n@ value and the bias
theoBRBias tau0 xs
  | V.length xs `div` ba < 100 = slowTheoBRBias tau0 xs
  | otherwise = fastTheoBRBias ba tau0 xs
  where
    -- We grow the Bias Average with the size of the data set.
    ba = integerCubeRoot (V.length xs)


-- | Fast TheoBR bias estimation, based on:
--
-- Taylor, Jennifer A. and David A. Howe.  2010.  "Fast TheoBR: A
-- Method for Long Data Set Stability Analysis."  /IEEE Transactions
-- on Ultrasonics, Ferroelectrics and Frequency Control/, Vol. 57,
-- No. 9 (Sep. 2010).
--
fastTheoBRBias
  :: forall v. (Vector v Double) =>
     Int          -- ^ Bias Average
  -> Tau0 Double  -- ^ Sampling interval
  -> v Double     -- ^ Time error samples
  -> Double       -- ^ The @n@ value and the bias
fastTheoBRBias ba tau0 xs = biasBA + 0.00055 * fromIntegral (ba - 1)
  where
    averages = chunkAvg ba xs
    biasBA = slowTheoBRBias (fromIntegral ba * tau0) averages
    

-- | Split the vector into chunks of length @ba@ and average each one.
chunkAvg :: (Vector v Double) => Int -> v Double -> v Double
{-# INLINE chunkAvg #-}
chunkAvg ba xs = V.generate (V.length xs `div` ba) term
  where term i = sumVector kbn (V.slice (i*ba) ba xs) / fromIntegral ba

-- | Regular TheoBR bias computation.  Very slow!
slowTheoBRBias
  :: (Vector v Double) =>
     Tau0 Double  -- ^ Sampling interval
  -> v Double     -- ^ Time error samples
  -> Double       -- ^ The @n@ value and the bias
slowTheoBRBias tau0 xs = summation "bias" 0 (n+1) term
  where n :: Int
        n = theoBR_N (V.length xs)

        term :: Int -> Double
        term i = avar tau0 (9 + 3*i) xs
               / unsafeTheo1var tau0 (12 + 4*i) xs



                           

--------------------------------------------------------------------
--------------------------------------------------------------------
--
-- TheoH
-- 

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

