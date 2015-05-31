module TauSigma.Util
       ( fromConsumer
       , fromProducer
       , toPipe
       ) where

import Control.Monad

import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P


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
