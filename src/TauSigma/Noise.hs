{-# LANGUAGE TemplateHaskell #-}

-- | Random noise generator supporting various spectral distributions
module TauSigma.Noise
       ( Options(..)
       , OutputType(..)
       , Mix(..)
       , main
       ) where

import Control.Monad (forever, replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.Random (MonadRandom, Random, getRandomR)

import Data.Csv (Only(..))
import Data.Default
import Data.Monoid

import Control.Lens
import Control.Lens.TH

import Pipes
import Pipes.ByteString (stdout)
import Pipes.Csv 
import qualified Pipes.Prelude as P


data Options =
  Options { _howMany :: Int
          , _outputType :: OutputType
          , _mix :: Mix
          }

instance Default Options where
  def = Options 1000 def def


data OutputType = Phase | Frequency

instance Default OutputType where
  def = Phase


-- | The mix of noise types to generate.
data Mix =
  Mix { -- | White frequency noise level.
        _wfm  :: Double
        -- | Flicker frequency noise level.
      , _ffm  :: Double
        -- | Random walk frequency noise level.
      , _rwfm :: Double
      }

instance Default Mix where
  def = Mix { _wfm  = 1.0
            , _ffm  = 0.0
            , _rwfm = 0.0
            }

$(makeLenses ''Options)
$(makeLenses ''Mix)


main :: (MonadRandom m, MonadIO m) => Options -> m ()
main opts =
  runEffect $ mixed opts
          >-> toOutputType (view outputType opts)
          >-> P.take (view howMany opts)
          >-> P.map Only
          >-> encode
          >-> stdout

-- TODO: don't pay the cost of noises at level 0.0
mixed :: MonadRandom m => Options -> Producer Double m ()
mixed opts = zipSum [ white (view (mix . wfm) opts)
                    , flicker octaves (view (mix . ffm) opts)
                    , brown (view (mix . rwfm) opts)
                    ]
  where octaves = floor $ logBase 2 (fromIntegral (view howMany opts))

-- | Handle the output type option.
toOutputType :: Monad m => OutputType -> Pipe Double Double m r
toOutputType Phase = integrate
toOutputType Frequency = cat

integrate :: (Monad m, Num a) => Pipe a a m r
integrate = P.scan (+) 0 id

-- | White noise is just random values.
white :: (MonadRandom m) => Double -> Producer Double m ()
white n = randomR (-n, n)

-- | Brown noise is integrated white noise.
brown :: (MonadRandom m) => Double -> Producer Double m ()
brown n = white n >-> integrate

flicker :: (MonadRandom m) => Int -> Double -> Producer Double m ()
flicker octaves n = zipSum (map go [1..(octaves+1)])
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


zipSum :: (Monad m, Num a) => [Producer a m ()] -> Producer a m ()
zipSum ps = foldr1 (P.zipWith (+)) ps
