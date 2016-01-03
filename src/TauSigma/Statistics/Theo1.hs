{-# LANGUAGE ScopedTypeVariables #-}

-- | Theo1 stability statistic.  See:
--
-- * http://tf.nist.gov/timefreq/general/pdf/1990.pdf
-- * http://tf.nist.gov/general/pdf/2220.pdf
module TauSigma.Statistics.Theo1
       ( Tau0
         
       , theo1var
       , theo1dev
       , theo1vars
       , theo1devs

       , theoBRvars
       , theoBRdevs
       , toTheoBRvars
       , toTheoBRdevs
       ) where

import Data.Maybe (fromJust)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

import TauSigma.Statistics.Allan (avars)
import TauSigma.Statistics.Util (Tau0, summation, allTaus)


theo1var :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> Maybe a
theo1var tau0 m xs
  | even m
    && 10 <= m
    && m <= V.length xs - 1 =
      Just (unsafeTheo1var tau0 m xs)
  | otherwise = Nothing

theo1dev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> Maybe a
theo1dev tau0 m xs = fmap sqrt (theo1var tau0 m xs)

theo1vars :: (Fractional a, Vector v a) => Tau0 -> v a -> IntMap a
theo1vars tau0 xs =
  allTaus (theo1Taus (V.length xs)) (unsafeTheo1var tau0) xs

theo1devs :: (Floating a, Vector v a) => Tau0 -> v a -> IntMap a
theo1devs tau0 xs = 
  allTaus (theo1Taus (V.length xs)) (unsafeTheo1dev tau0) xs

theo1Taus :: Int -> [Int]
theo1Taus size = filter even [10..limit]
  where limit = 3 * (size `div` 4)


-- | This is a worse than a partial function: it's a function that
-- produces incorrect results for some of its arguments.  Stick to
-- 'theo1vars' unless you really know what you're doing.
unsafeTheo1var :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
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

unsafeTheo1dev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
unsafeTheo1dev tau0 m xs = sqrt (unsafeTheo1var tau0 m xs)

-- | Bias-reduced Theo1 variance.  This computes 'avars' and
-- 'theo1vars', so if you're doing that anyway you may wish to use
-- 'toTheoBRvars' which reuses the memoized results of those two.
theoBRvars :: (RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
theoBRvars tau0 xs = toTheoBRvars (V.length xs) allans theo1s
  where allans = avars tau0 xs
        theo1s = theo1vars tau0 xs

theoBRdevs :: (Floating a, RealFrac a, Vector v a) => Tau0 -> v a -> IntMap a
theoBRdevs tau0 xs = IntMap.map sqrt (theoBRvars tau0 xs)


toTheoBRvars
  :: forall a. RealFrac a =>
     Int        -- ^ The length of the phase point data set
  -> IntMap a   -- ^ The 'avars' result
  -> IntMap a   -- ^ The 'theo1vars' result
  -> IntMap a
toTheoBRvars size allans theo1s = IntMap.mapWithKey go theo1s
  where n :: Int
        n = floor (((0.1 * fromIntegral size) / 3) - 3)

        go :: Int -> a -> a
        go _ theo1AtM = (1 / fromIntegral (n+1)) * ratio * theo1AtM 
          where ratio :: a
                ratio = summation 0 n term
                  where term :: Int -> a
                        term i = theAvar / theTheo1
                          where unsafe :: Int -> IntMap a -> a
                                {-# INLINE unsafe #-}
                                unsafe k m = fromJust (IntMap.lookup k m)
                                theAvar :: a
                                theAvar  = unsafe (9 + 3*i) allans
                                theTheo1 :: a
                                theTheo1 = unsafe (12 + 4*i) theo1s

toTheoBRdevs
  :: forall a. (Floating a, RealFrac a) =>
     Int        -- ^ The length of the phase point data set
  -> IntMap a   -- ^ The 'avars' result
  -> IntMap a   -- ^ The 'theo1vars' result
  -> IntMap a
toTheoBRdevs size allans theo1s =
  IntMap.map sqrt (toTheoBRvars size allans theo1s)
