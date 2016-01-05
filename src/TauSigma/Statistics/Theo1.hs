{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Theo1 stability statistic.  See:
--
-- * http://tf.nist.gov/timefreq/general/pdf/1990.pdf
-- * http://tf.nist.gov/general/pdf/2220.pdf
module TauSigma.Statistics.Theo1
       ( Tau0

       , isTheo1Tau
         
       , theo1var
       , theo1dev
       , theo1vars
       , theo1devs

       , theoBRvars
       , theoBRdevs
       , toTheoBRvars
       , toTheoBRdevs
       ) where

import Data.Default (Default)
import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Allan (avars)
import TauSigma.Statistics.Util

import TauSigma.Util.DenseIntMap (DenseIntMap, Entry(..), entries)
import qualified TauSigma.Util.DenseIntMap as IntMap


-- | Theo1 is only defined for certain sampling intervals.
isTheo1Tau :: Int -> Int -> Bool
isTheo1Tau m size = even m && 10 <= m && m <= size - 1

theo1var :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> Maybe a
{-# INLINABLE theo1var #-}
theo1var tau0 m xs
  | m `isTheo1Tau` V.length xs =
      Just (unsafeTheo1var tau0 m xs)
  | otherwise = Nothing

theo1dev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> Maybe a
{-# INLINABLE theo1dev #-}
theo1dev tau0 m xs = fmap sqrt (theo1var tau0 m xs)

theo1vars
  :: (Fractional a, Default a, Vector v a, Vector v (Entry a)) =>
     Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE theo1vars #-}
theo1vars tau0 xs = IntMap.fromEntries (V.generate (taus + 1) go)
  where size = V.length xs
        taus = 3 * (size `div` 4)
        go m
          | m `isTheo1Tau` size = Entry True (unsafeTheo1var tau0 m xs)
          | otherwise           = Entry False 0.0

theo1devs
  :: (Floating a, Default a, Vector v a, Vector v (Entry a)) =>
     Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE theo1devs #-}
theo1devs tau0 xs = IntMap.map sqrt (theo1vars tau0 xs)


-- | This is a worse than a partial function: it's a function that
-- produces incorrect results for some of its arguments.  Stick to
-- 'theo1vars' unless you really know what you're doing.
unsafeTheo1var
  :: (Fractional a, Vector v a) =>
     Tau0 -> Int -> v a -> a
{-# INLINABLE unsafeTheo1var #-}
unsafeTheo1var tau0 m xs = outer / (0.75 * fromIntegral divisor)
  where divisor :: Integer
        divisor = (len - m') * (m' * tau0')^2
          where m' = fromIntegral m
                tau0' = fromIntegral tau0
                len = fromIntegral (V.length xs)
        outer = summation 0 (V.length xs - m) middle
          where middle i = summation 0 (m `div` 2) inner
                  where inner d = term^2 / fromIntegral (halfM - d)
                          where halfM = m `div` 2
                                term = (xs!i - xs!(i - d + halfM))
                                     + (xs!(i+m) -xs!(i + d + halfM))

unsafeTheo1dev
  :: (Floating a, Default a, Vector v a) => Tau0 -> Int -> v a -> a
{-# INLINABLE unsafeTheo1dev #-}
unsafeTheo1dev tau0 m xs = sqrt (unsafeTheo1var tau0 m xs)

-- | Bias-reduced Theo1 variance.  This computes 'avars' and
-- 'theo1vars', so if you're doing that anyway you may wish to use
-- 'toTheoBRvars' which reuses the memoized results of those two.
theoBRvars
  :: (RealFrac a, Default a, Vector v a, Vector v (Entry a)) =>
     Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE theoBRvars #-}
theoBRvars tau0 xs = toTheoBRvars (V.length xs) allans theo1s
  where allans = avars tau0 xs
        theo1s = theo1vars tau0 xs

theoBRdevs
  :: (Floating a, RealFrac a, Default a, Vector v a, Vector v (Entry a)) =>
     Tau0 -> v a -> DenseIntMap v a
{-# INLINABLE theoBRdevs #-}
theoBRdevs tau0 xs = IntMap.map sqrt (theoBRvars tau0 xs)


toTheoBRvars
  :: forall v a. (RealFrac a, Default a, Vector v (Entry a)) =>
     Int               -- ^ The length of the phase point data set
  -> DenseIntMap v a   -- ^ The 'avars' result
  -> DenseIntMap v a   -- ^ The 'theo1vars' result
  -> DenseIntMap v a
{-# INLINABLE toTheoBRvars #-}
toTheoBRvars size allans theo1s = IntMap.map go theo1s
  where go :: a -> a
        go theo1 = (ratio * theo1) / fromIntegral (n+1)

        n :: Int
        -- From Howe & Tasset:
        n = floor (((0.1 * fromIntegral size) / 3) - 3)
{- Riley has this instead:

        n = floor (fromIntegral size / 6 - 3)

   Which is way slower and for some tests produces Maybe.fromJust: Nothing
   errors in the ratio code below.
-}

        ratio :: a
        ratio = summation 0 (n+1) term
          where term :: Int -> a
                term i = theAvar / theTheo1
                  where unsafe :: Int -> DenseIntMap v a -> a
                        unsafe k m = case entries m ! k of
                                      Entry True v -> v
                                      _ -> error "accessed bad entry"
                        theAvar :: a
                        theAvar  = unsafe (9 + 3*i) allans
                        theTheo1 :: a
                        theTheo1 = unsafe (12 + 4*i) theo1s

toTheoBRdevs
  :: (Floating a, RealFrac a, Default a, Vector v (Entry a)) =>
     Int               -- ^ The length of the phase point data set
  -> DenseIntMap v a   -- ^ The 'avars' result
  -> DenseIntMap v a   -- ^ The 'theo1vars' result
  -> DenseIntMap v a
{-# INLINABLE toTheoBRdevs #-}
toTheoBRdevs size allans theo1s =
  IntMap.map sqrt (toTheoBRvars size allans theo1s)
