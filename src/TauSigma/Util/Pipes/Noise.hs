-- | Utility pipes for generating various types of noise.
module TauSigma.Util.Pipes.Noise
    ( white
    , brown
    , flicker
    , randomR
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
    ) where

import Control.Monad (forever, replicateM_)
import Control.Monad.Random (MonadRandom, Random, getRandomR)

import Data.Tagged
import Pipes
import qualified Pipes.Prelude as P

import TauSigma.Types


-- | White noise is just random values.
white :: MonadRandom m => Double -> Producer Double m ()
white n = randomR (-n, n)

-- | Brown noise is integrated white noise.
brown :: (MonadRandom m) => Double -> Producer Double m ()
brown n = white n >-> integrate

-- | Flicker noise has 1/f power density, i.e., inversely proportional
-- to the frequency.  
flicker :: (MonadRandom m) => Int -> Double -> Producer Double m ()
flicker octaves n = zipSum (map go [0..octaves])
  where go o = white n >-> hold o

-- | Hold a signal for @2^octave@ ticks.  (Yes, higher number = lower
-- octave.)
hold :: Monad m => Int -> Pipe a a m r
hold octave = forever $ do
  a <- await
  replicateM_ (2^octave) (yield a)


-- | Ranged random 'Producer'.
randomR :: (MonadRandom m, Random a) => (a, a) -> Producer a m ()
randomR (lo, hi) = forever (lift (getRandomR (lo, hi)) >>= yield)


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



whitePhase :: MonadRandom m => Double -> Producer (TimeData Double) m ()
whitePhase n = white n >-> P.map Tagged

flickerPhase
  :: MonadRandom m =>
     Int -> Double -> Producer (TimeData Double) m ()
flickerPhase octaves n = flicker octaves n >-> P.map Tagged

whiteFrequency :: MonadRandom m => Double -> Producer (FreqData Double) m ()
whiteFrequency n = white n >-> P.map Tagged

flickerFrequency
  :: MonadRandom m => Int -> Double -> Producer (FreqData Double) m ()
flickerFrequency octaves n = flicker octaves n >-> P.map Tagged

randomWalkFrequency
  :: MonadRandom m => Double -> Producer (FreqData Double) m ()
randomWalkFrequency n = brown n >-> P.map Tagged
