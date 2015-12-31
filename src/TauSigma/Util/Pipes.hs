module TauSigma.Util.Pipes
       ( fromConsumer
       , fromProducer
       , toPipe

       , toPhase
       , integrate
       , toFrequency
       , differentiate
       ) where

import Data.Tagged
import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P

import TauSigma.Types


-- | Turn a 'Consumer' into a 'Pipe' that consumes the same input and
-- yields the 'Consumer's result.
fromConsumer :: Monad m => Consumer i m o -> Pipe i o m ()
fromConsumer c = c //> closed >>= yield

-- | Convert a 'Producer' into a 'Pipe' that ignores its upstream
-- and sends the producer's contents downstream.
fromProducer :: Monad m => Producer o m r -> Pipe i o m r
fromProducer p = closed >\\ p

toPipe :: Monad m => Consumer i m r -> (r -> Producer o m ()) -> Pipe i o m ()
toPipe c p = fromConsumer c `for` (fromProducer . p)


-- | Convert a sequence of frequency points to phase points.
toPhase :: (Monad m, Num a) => Pipe (FreqData a) (TimeData a) m r
toPhase = P.map unTagged >-> integrate >-> P.map Tagged

-- | Integrate a sequence of data points (i.e., take the running sum).
integrate :: (Monad m, Num a) => Pipe a a m r
integrate = P.scan (+) 0 id

-- | Convert sequence of phase points to frequencies.
toFrequency :: (Monad m, Num a) => Pipe (TimeData a) (FreqData a) m r
toFrequency = P.map unTagged >-> differentiate >-> P.map Tagged

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

