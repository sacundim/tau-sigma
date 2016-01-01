{-# LANGUAGE TemplateHaskell #-}

-- | Random noise generator supporting various spectral distributions
module TauSigma.Noise
       ( Options(..)
       , Domain(..)
       , Mix(..)
       , options
       , main
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Random (MonadRandom)

import Data.Csv (Only(..))
import Data.Default
import Data.Maybe (catMaybes)
import Data.Monoid

import Options.Applicative

import Pipes
import Pipes.ByteString (stdout)
import Pipes.Csv 
import qualified Pipes.Prelude as P

import TauSigma.Types
import TauSigma.Util.Pipes.Noise

options :: Parser Options
options = Options
      <$> option auto
          ( long "length"
         <> short 'n'
         <> metavar "N"
         <> value 1000
         <> help "Generate N data points. Default: 1000"
          )
      <*> flag Phase Frequency
          ( long "frequency"
         <> help "Output as frequency data instead of phase"
          )
       <*> ( Mix
         <$> option (fmap Just auto)
             ( long "wpm"
            <> metavar "N"
            <> value Nothing
            <> help "White phase noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "fpm"
            <> metavar "N"
            <> value Nothing
            <> help "Flicker phase noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "wfm"
            <> metavar "N"
            <> value Nothing
            <> help "White frequency noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "ffm"
            <> metavar "N"
            <> value Nothing
            <> help "Flicker frequency noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "rwfm"
            <> metavar "N"
            <> value Nothing
            <> help "Random walk frequency noise intensity"
             )
           )
         
data Options =
  Options { _howMany :: Int
          , _outputType :: Domain
          , _mix :: Mix
          }

-- | The mix of noise types to generate.
data Mix =
  Mix { -- | White phase noise level.
        _wpm  :: Maybe Double
        -- | Flicker phase noise level.
      , _fpm  :: Maybe Double
        -- | White frequency noise level.
      , _wfm  :: Maybe Double
        -- | Flicker frequency noise level.
      , _ffm  :: Maybe Double
        -- | Random walk frequency noise level.
      , _rwfm :: Maybe Double
      }

instance Default Mix where
  def = Mix Nothing Nothing (Just 1.0) Nothing Nothing

$(makeLenses ''Options)
$(makeLenses ''Mix)



main :: (MonadRandom m, MonadIO m) => Options -> m ()
main opts =
  runEffect $ mixed (applyDefaults opts)
          >-> toOutputType (view outputType opts)
          >-> P.take (view howMany opts)
          >-> P.map Only
          >-> encode
          >-> stdout

applyDefaults :: Options -> Options
applyDefaults opts = over mix go opts
  where go (Mix Nothing Nothing Nothing Nothing Nothing) = def
        go other = other

--- | Handle the output type option.
toOutputType :: Monad m => Domain -> Pipe (TimeData Double) Double m r
toOutputType Phase = P.map unTagged
toOutputType Frequency = toFrequency >-> P.map unTagged

mixed :: MonadRandom m => Options -> Producer (TimeData Double) m ()
mixed opts =
  zipSum $ catMaybes [ auxT whitePhase wpm
                     , auxT (flickerPhase octaves) fpm
                     , auxF whiteFrequency wfm 
                     , auxF (flickerFrequency octaves) ffm 
                     , auxF randomWalkFrequency rwfm 
                     ]
  where auxT f g = f <$> view (mix . g) opts
        auxF f g = fmap (>-> toPhase) (auxT f g)
        octaves = floor $ logBase 2 (fromIntegral (view howMany opts))
        

