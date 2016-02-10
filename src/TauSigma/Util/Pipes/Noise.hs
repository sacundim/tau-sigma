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

-- CONFUSING: 'MonadPrim' (from 'Control.Monad.Primitive.Class') is not the
-- same class as 'PrimMonad' (from 'Control.Monad.Primitive')!!!
import Control.Monad.Primitive 
import Control.Monad.Primitive.Class (MonadPrim)

import Data.Tagged
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC.Monad (Rand)
import qualified System.Random.MWC.Monad as Rand
import qualified System.Random.MWC.Distributions.Monad as Dist

import TauSigma.Types


-- | White noise is normally distributed with a standard deviation of @n@.
white :: MonadPrim m => Double -> Producer Double (Rand m) ()
{-# INLINABLE white #-} 
white n = forever (lift (Dist.normal 0.0 n) >>= yield)

-- | Brown noise is integrated white noise.
brown :: MonadPrim m => Double -> Producer Double (Rand m) ()
{-# INLINABLE brown #-} 
brown n = white n >-> integrate

-- | Flicker noise has 1/f power density, i.e., inversely proportional
-- to the frequency.  This uses a variant of the Voss-McCartney
-- algorithm.  Sources:
--
-- * http://www.firstpr.com.au/dsp/pink-noise/#Stochastic_Voss_McCartney
-- * http://home.earthlink.net/%7Eltrammell/tech/pinkalg.htm
flicker
    :: forall m. (MonadPrim m, PrimMonad m) =>
       Int
    -> Double
    -> Producer Double (Rand m) ()
{-# INLINABLE flicker #-} 
flicker octaves n = lift (MU.replicate (2*octaves) 0.0) >>= go
  where
    go :: MU.MVector (PrimState m) Double -> Producer Double (Rand m) ()
    go state =
        forever $ do
          i <- lift (decaying octaves)
          ri <- lift (Dist.normal 0.0 n)
          lift (MU.write state i ri)
          next <- lift (sumMVector state)
          r <- lift (Dist.normal 0.0 n)
          yield (r + next)

decaying :: MonadPrim m => Int -> Rand m Int
decaying n = go 0
    where go i | i < n = do b <- Rand.uniform
                            if b then return i else go (i+1)
               | otherwise = return i

sumMVector :: PrimMonad m => MU.MVector (PrimState m) Double -> m Double
{-# INLINE sumMVector #-} 
sumMVector v = fmap U.sum (U.unsafeFreeze v)
      

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



whitePhase :: MonadPrim m => Double -> Producer (TimeData Double) (Rand m) ()
{-# INLINABLE whitePhase #-} 
whitePhase n = white n >-> P.map Tagged

flickerPhase
  :: (MonadPrim m, PrimMonad m) =>
     Int -> Double -> Producer (TimeData Double) (Rand m) ()
{-# INLINABLE flickerPhase #-} 
flickerPhase octaves n = flicker octaves n >-> P.map Tagged

whiteFrequency
  :: MonadPrim m => Double -> Producer (FreqData Double) (Rand m) ()
{-# INLINABLE whiteFrequency #-} 
whiteFrequency n = white n >-> P.map Tagged

flickerFrequency
  :: (MonadPrim m, PrimMonad m) =>
     Int -> Double -> Producer (FreqData Double) (Rand m) ()
{-# INLINABLE flickerFrequency #-} 
flickerFrequency octaves n = flicker octaves n >-> P.map Tagged

randomWalkFrequency
  :: MonadPrim m => Double -> Producer (FreqData Double) (Rand m) ()
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


instance PrimMonad m => PrimMonad (Rand m) where
  type PrimState (Rand m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
