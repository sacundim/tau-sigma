-- | Utility pipes for generating various types of noise.
module TauSigma.Util.Pipes.Noise
    ( white
    , brown
    , flicker
    , hold
    , zipSum
    , integrate
    , differentiate
    , toFrequency
    , toPhase
    , whitePhase
    , flickerPhase
    , whiteFrequency
    , flickerFrequency
    , randomWalkFrequency

    -- Re-exports
    , MonadPrim
    , Rand
    ) where

import Control.Monad (forever, replicateM_)

-- CONFUSING: 'MonadPrim' (from 'Control.Monad.Primitive.Class') is not the
-- same class as 'PrimMonad' (from 'Control.Monad.Primitive')!!!
import Control.Monad.Primitive.Class (MonadPrim)

import Data.Tagged
import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC.Monad (Rand)
import qualified System.Random.MWC.Monad as Random
import qualified System.Random.MWC.Distributions.Monad as Dist

import TauSigma.Types


-- | White noise is normally distributed with a standard deviation of @n@.
white :: MonadPrim m => Double -> Producer Double (Rand m) ()
white n = forever (lift (Dist.normal 0.0 n) >>= yield)

-- | Brown noise is integrated white noise.
brown :: MonadPrim m => Double -> Producer Double (Rand m) ()
brown n = white n >-> integrate

-- | Flicker noise has 1/f power density, i.e., inversely proportional
-- to the frequency.  
flicker :: MonadPrim m => Int -> Double -> Producer Double (Rand m) ()
flicker octaves n = zipSum (map go [0..octaves])
  where go o = white n >-> hold o

-- | Hold a signal for @2^octave@ ticks.  (Yes, higher number = lower
-- octave.)
hold :: Monad m => Int -> Pipe a a m r
hold octave = forever $ do
  a <- await
  replicateM_ (2^octave) (yield a)


-- | Zip the given pipes, adding their outputs.
zipSum :: (Monad m, Num a) => [Producer a m ()] -> Producer a m ()
zipSum ps = foldr1 (P.zipWith (+)) ps

-- | Integrate a sequence of data points (i.e., take the running sum).
integrate :: (Monad m, Num a) => Pipe a a m r
integrate = P.scan (+) 0 id

-- | Integrate a sequence of data points (i.e., take differences of
-- consecutive items).
differentiate :: (Monad m, Num a) => Pipe a a m r
differentiate = await >>= differentiate'
  where
    differentiate' :: (Monad m, Num a) => a -> Pipe a a m r
    differentiate' prev = do
      cur <- await
      yield (cur - prev)
      differentiate' cur



-- | Convert a sequence of frequency points to phase points.
toPhase :: (Monad m, Num a) => Pipe (FreqData a) (TimeData a) m r
toPhase = P.map unTagged >-> integrate >-> P.map Tagged

-- | Convert sequence of phase points to frequencies.
toFrequency :: (Monad m, Num a) => Pipe (TimeData a) (FreqData a) m r
toFrequency = P.map unTagged >-> differentiate >-> P.map Tagged



whitePhase :: MonadPrim m => Double -> Producer (TimeData Double) (Rand m) ()
whitePhase n = white n >-> P.map Tagged

flickerPhase
  :: MonadPrim m =>
     Int -> Double -> Producer (TimeData Double) (Rand m) ()
flickerPhase octaves n = flicker octaves n >-> P.map Tagged

whiteFrequency
  :: MonadPrim m => Double -> Producer (FreqData Double) (Rand m) ()
whiteFrequency n = white n >-> P.map Tagged

flickerFrequency
  :: MonadPrim m => Int -> Double -> Producer (FreqData Double) (Rand m) ()
flickerFrequency octaves n = flicker octaves n >-> P.map Tagged

randomWalkFrequency
  :: MonadPrim m => Double -> Producer (FreqData Double) (Rand m) ()
randomWalkFrequency n = brown n >-> P.map Tagged
