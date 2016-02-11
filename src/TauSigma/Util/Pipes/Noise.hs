{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utility pipes for generating various types of noise.
module TauSigma.Util.Pipes.Noise
    ( white
    , brown
    , flicker
    , octaves
    , sinusoid

    , whitePhase
    , flickerPhase
    , whiteFrequency
    , flickerFrequency
    , randomWalkFrequency
    , tourbillonFrequency

    , TimeData
    , FreqData
    , toFrequency
    , toPhase

    , zipSum
    , integrate
    , differentiate
    ) where

import Control.Monad (forever)
import Control.Monad.Primitive (PrimMonad(..))

import Data.Bits (countLeadingZeros)
import Data.Word (Word64)
import Data.Tagged
    
import Data.Random (RVarT, stdUniformT, normalT)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Pipes
import qualified Pipes.Prelude as P

import TauSigma.Types


-- | White noise is normally distributed with a standard deviation of @n@.
white :: Monad m => Double -> Producer Double (RVarT m) ()
{-# INLINABLE white #-} 
white n = forever (lift (normalT 0.0 n) >>= yield)

-- | Brown noise is integrated white noise.
brown :: Monad m => Double -> Producer Double (RVarT m) ()
{-# INLINABLE brown #-} 
brown n = white n >-> integrate

-- | Flicker noise has 1/f power density, i.e., inversely proportional
-- to the frequency.  This uses a variant of the Voss-McCartney
-- algorithm.  Sources:
--
-- * http://www.firstpr.com.au/dsp/pink-noise/#Stochastic_Voss_McCartney
-- * http://home.earthlink.net/%7Eltrammell/tech/pinkalg.htm
flicker
    :: forall m. (PrimMonad m) =>
       Int
    -> Double
    -> Producer Double (RVarT m) ()
{-# INLINABLE flicker #-} 
flicker octaves n = lift (MU.replicate octaves 0.0) >>= flicker' 
    where flicker' state = go 0.0
              where go prev = do
                      i   <- lift (decaying octaves)
                      ri  <- lift (normalT 0.0 n)
                      ri' <- lift (write' state i ri)
                      let next = prev - ri' + ri 
                      r   <- lift (normalT 0.0 n)
                      yield (next + r)
                      go next

-- | Choose an @i@ in the range @[0,n)@, with probability @0.5^(i+1)@.
-- Well, except that @n-1@ gets picked inordinately often.
decaying :: forall m. Monad m => Int -> RVarT m Int
{-# INLINE decaying #-} 
decaying n = fmap (min (n-1) . countLeadingZeros) word
   where word :: RVarT m Word64
         word = stdUniformT

-- | Write to a mutable vector, but returning the value that was replaced.
write'
    :: (PrimMonad m, U.Unbox a) =>
       MU.MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE write' #-}
write' v i a = MU.read v i <* MU.write v i a
         


-- | Calculate the number of octaves in a sequence of the given size.
-- Suitable for passing as argument to 'flicker' and 'flickerFrequency'.
octaves :: Int -> Int
octaves size = floor $ logBase 2 (fromIntegral size)

-- | Generate a unit sinusoid signal with the given period.
sinusoid :: (Monad m, Floating a) => Int -> a -> Producer a m r
{-# INLINABLE sinusoid #-} 
sinusoid period level = cycle period >-> P.map step
  where period' = 2*pi / fromIntegral period 
        step i = level * sin (fromIntegral i * period')
        cycle period = go 0
          where go i = yield (i `rem` period) >> go (succ i)



whitePhase :: Monad m => Double -> Producer (TimeData Double) (RVarT m) ()
{-# INLINABLE whitePhase #-} 
whitePhase n = white n >-> P.map Tagged

flickerPhase
  :: PrimMonad m =>
     Int -> Double -> Producer (TimeData Double) (RVarT m) ()
{-# INLINABLE flickerPhase #-} 
flickerPhase octaves n = flicker octaves n >-> P.map Tagged

whiteFrequency
  :: Monad m => Double -> Producer (FreqData Double) (RVarT m) ()
{-# INLINABLE whiteFrequency #-} 
whiteFrequency n = white n >-> P.map Tagged

flickerFrequency
  :: PrimMonad m =>
     Int -> Double -> Producer (FreqData Double) (RVarT m) ()
{-# INLINABLE flickerFrequency #-} 
flickerFrequency octaves n = flicker octaves n >-> P.map Tagged

randomWalkFrequency
  :: Monad m => Double -> Producer (FreqData Double) (RVarT m) ()
{-# INLINABLE randomWalkFrequency #-} 
randomWalkFrequency n = brown n >-> P.map Tagged

tourbillonFrequency
  :: Monad m => Int -> Double -> Producer (FreqData Double) m ()
{-# INLINABLE tourbillonFrequency #-} 
tourbillonFrequency period n = sinusoid period n >-> P.map Tagged


-- | Convert a sequence of frequency points to phase points.
toPhase :: (Monad m, Num a) => Pipe (FreqData a) (TimeData a) m r
{-# INLINABLE toPhase #-}
toPhase = P.map unTagged >-> integrate >-> P.map Tagged

-- | Convert sequence of phase points to frequencies.
toFrequency :: (Monad m, Num a) => Pipe (TimeData a) (FreqData a) m r
{-# INLINABLE toFrequency #-}
toFrequency = P.map unTagged >-> differentiate >-> P.map Tagged


-- | Zip the given pipes, adding their outputs.
zipSum :: (Monad m, Num a) => [Producer a m ()] -> Producer a m ()
{-# INLINE zipSum #-}
zipSum ps = foldr1 (P.zipWith (+)) ps

-- | Integrate a sequence of data points (i.e., take the running sum).
integrate :: (Monad m, Num a) => Pipe a a m r
{-# INLINE integrate #-}
integrate = P.scan (+) 0 id

-- | Integrate a sequence of data points (i.e., take differences of
-- consecutive items).
differentiate :: (Monad m, Num a) => Pipe a a m r
{-# INLINE differentiate #-}
differentiate = await >>= differentiate'
  where
    differentiate' :: (Monad m, Num a) => a -> Pipe a a m r
    {-# INLINE differentiate' #-}
    differentiate' prev = do
      cur <- await
      yield (cur - prev)
      differentiate' cur


{- TODO: find out if I can get rid of this -}
instance PrimMonad m => PrimMonad (RVarT m) where
    type PrimState (RVarT m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}
