{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

-- | Allan Variance and other related frequency stability statistics.
--
-- See: http://tf.nist.gov/general/pdf/2220.pdf
--
module Statistics.Allan
       ( integrate
       , avar
       , avars
       , adev
       , adevs
       ) where

import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V

type Tau0 = Int


-- | All the functions in this module take phase error sequences as
-- input.  This function converts frequency error data to phase error
-- data.
integrate :: (Num a, Vector v a) => v a -> v a
integrate xs = V.scanl' (+) 0 xs


avar :: forall v a. (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
avar tau0 m xs = sumsq / fromIntegral divisor
  where divisor = 2 * m^2 * tau0^2 * (V.length xs - 2*m)
        sumsq = V.sum (V.generate (V.length xs - 2*m) step :: v a)
          where step i = (xs!(i+2*m) - 2*(xs!(i+m)) + xs!i)^2

adev :: (Floating a, Vector v a) => Tau0-> Int -> v a -> a
adev tau0 m xs = sqrt (avar tau0 m xs)

avars :: (Fractional a, Vector v a) => Tau0 -> v a -> [(Int, a)]
avars tau0 xs = map step [1..maxTaus]
  where step m = (m, avar tau0 m xs)
        maxTaus = V.length xs `div` 5
                    
adevs :: (Floating a, Vector v a) => Tau0 -> v a -> [(Int, a)]
adevs tau0 xs = map step (avars tau0 xs)
  where step (m, s) = (m, sqrt s)


-- Below here is untested yet...

theo1 :: forall v a. (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
theo1 tau0 m xs
  | even m && 10 <= m && m < V.length xs = sumsq / divisor
  where divisor = fromIntegral ((V.length xs - m) * (m * tau0)^2)
        sumsq = V.sum (V.generate (V.length xs - m) outer :: v a)
        outer i = V.sum (V.generate (m `div` 2) inner :: v a)
          where inner d = ((xs!i - xs!(i - d + (m `div` 2)))
                               + (xs!(i+m) - xs!(i + d + (m `div` 2))))^2
                          / divisor'
                  where divisor' = fromIntegral $ (m `div` 2) - d

theo1s :: (Fractional a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theo1s tau0 xs = map step taus
  where step m = (m, theo1 tau0 m xs)
        taus = [m | m <- [10 .. (V.length xs `div` 4) * 3], even m]

theo1devs :: (Floating a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theo1devs tau0 xs = map step (theo1s tau0 xs)
  where step (m, s) = (m, sqrt s)


theoBR :: forall v a. (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
theoBR tau0 m xs | even m = (theSum / (fromIntegral n + 1)) * theo1 tau0 m xs
  where n = floor (fromIntegral (V.length xs) / 6 - 3)
        theSum = V.sum (V.generate (n+1) step :: v a)
        step i = avar tau0 (9 + 3*i) xs / theo1 tau0 (12 + 4*i) xs

theoBRs :: (Fractional a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theoBRs tau0 xs = map step taus
  where step m = (m, theoBR tau0 m xs)
        taus = [m | m <- [10 .. (V.length xs `div` 4) * 3], even m]

theoBRdevs :: (Floating a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theoBRdevs tau0 xs = map step (theoBRs tau0 xs)
  where step (m, s) = (m, sqrt s)


theoHs :: (Fractional a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theoHs tau0 xs = avars tau0 xs ++ theoBRs tau0 xs

theoHdevs :: (Floating a, Vector v a) => Tau0 -> v a -> [(Int, a)]
theoHdevs tau0 xs = map step (theoHs tau0 xs)
  where step (m, s) = (m, sqrt s)

theoH :: (Fractional a, Vector v a) => Tau0 -> Int -> v a -> a
theoH tau0 m xs
  | m <= (k `div` tau0) = avar tau0 m xs
  | (fromIntegral k / (0.75 * fromIntegral tau0) <= fromIntegral m)
 && (m <= V.length xs - 1) && even m =
      theoBR tau0 m xs
  | otherwise = 0.0 / 0.0
  where k = (V.length xs `div` 5) * tau0

theoHdev :: (Floating a, Vector v a) => Tau0 -> Int -> v a -> a
theoHdev tau0 m xs = sqrt (theoH tau0 m xs)

