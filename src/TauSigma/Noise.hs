{-# LANGUAGE TemplateHaskell #-}

-- | Random noise generator supporting various spectral distributions
module TauSigma.Noise
       ( Options(..)
       , OutputType(..)
       , Mix(..)
       , main
       ) where

import Control.Monad (forever)
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
  Mix {
        -- | White phase noise level.
        _wpm  :: Double  
        -- | Flicker phase noise level.
      , _fpm  :: Double
        -- | White frequency noise level.
      , _wfm  :: Double
        -- | Flicker frequency noise level.
      , _ffm  :: Double
        -- | Random walk frequency noise level.
      , _rwfm :: Double
      }

instance Default Mix where
  def = Mix { _wpm  = 0.0
            , _fpm  = 0.0
            , _wfm  = 1.0
            , _ffm  = 0.0
            , _rwfm = 0.0
            }

$(makeLenses ''Options)
$(makeLenses ''Mix)


main :: (MonadRandom m, MonadIO m) => Options -> m ()
main opts =
  runEffect $ mixed (view mix opts)
          >-> toOutputType (view outputType opts)
          >-> P.take (view howMany opts)
          >-> P.map Only
          >-> encode
          >-> stdout

mixed :: MonadRandom m => Mix -> Producer Double m ()
mixed mix = P.zipWith (+) (white (view wfm mix)) (brown (view rwfm mix))

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

{-
flicker :: (MonadRandom m) => Double -> Producer Double m ()
flicker n = _
-}

-- | Ranged random 'Producer'.
randomR :: (MonadRandom m, Random a) => (a, a) -> Producer a m ()
randomR (lo, hi) = forever (lift (getRandomR (lo, hi)) >>= yield)
