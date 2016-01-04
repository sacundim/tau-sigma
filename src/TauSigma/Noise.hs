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

-- CONFUSING: 'MonadPrim' (from 'Control.Monad.Primitive.Class') is not the
-- same class as 'PrimMonad' (from 'Control.Monad.Primitive')!!!
import Control.Monad.Primitive.Class (MonadPrim)

import Data.Functor.Compose (Compose(..))

import Data.Csv (Only(..))
import Data.Default
import Data.Maybe (catMaybes)
import Data.Monoid

import Options.Applicative

import Pipes
import Pipes.ByteString (stdout)
import Pipes.Csv 
import qualified Pipes.Prelude as P

import System.Random.MWC.Monad (Rand, runWithSystemRandomT)

import TauSigma.Types
import TauSigma.Util.Pipes.Noise

options :: Parser Options
options = Options <$> length <*> frequency <*> mix
  where
    f `with` xs = f (mconcat xs)
    length = option auto
             `with` [ long "length"
                    , short 'n'
                    , metavar "N"
                    , value 1000
                    , help "Generate N data points. Default: 1000"
                    ]
    frequency = flag Phase Frequency `with`
                [ long "frequency"
                , help "Output as frequency data instead of phase"
                ]
    mix = Mix <$> wpm <*> fpm <*> wfm <*> ffm <*> rwfm <*> getCompose tourbillon
      where
        wpm = option (fmap Just auto)
              `with` [ long "wpm"
                     , metavar "N"
                     , value Nothing
                     , help "White phase noise intensity"
                     ]
        fpm = option (fmap Just auto) 
              `with` [ long "fpm"
                     , metavar "N"
                     , value Nothing
                     , help "Flicker phase noise intensity"
                     ]
        wfm = option (fmap Just auto)
              `with` [ long "wfm"
                     , metavar "N"
                     , value Nothing
                     , help "White frequency noise intensity"
                     ]
        ffm = option (fmap Just auto)
              `with` [ long "ffm"
                     , metavar "N"
                     , value Nothing
                     , help "Flicker frequency noise intensity"
                     ]
        rwfm = option (fmap Just auto)
               `with` [ long "rwfm"
                      , metavar "N"
                      , value Nothing
                      , help "Random walk frequency noise intensity"
                      ]
        tourbillon :: Compose Parser Maybe (Int, Double)
        tourbillon = (,) <$> period <*> level
          where
            period = Compose $ option (fmap Just auto)
                     `with` [ long "tourbillon-period"
                            , metavar "N"
                            , value Nothing
                            ]
            level = Compose $ option (fmap Just auto)
                    `with` [ long "tourbillon-level"
                           , metavar "N"
                           , value (Just 1.0)
                           ]
                  
                  
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
        -- | Tourbillon period and intensity.
      , _tourbillon :: Maybe (Int, Double)
      }

instance Default Mix where
  def = Mix Nothing Nothing (Just 1.0) Nothing Nothing Nothing

$(makeLenses ''Options)
$(makeLenses ''Mix)



main :: Options -> IO ()
main opts =
  runWithSystemRandomT $ runEffect $ mixed (applyDefaults opts)
          >-> toOutputType (view outputType opts)
          >-> P.take (view howMany opts)
          >-> P.map Only
          >-> encode
          >-> stdout

applyDefaults :: Options -> Options
applyDefaults opts = over mix go opts
  where go (Mix Nothing Nothing Nothing Nothing Nothing Nothing) = def
        go other = other

--- | Handle the output type option.
toOutputType :: Monad m => Domain -> Pipe (TimeData Double) Double m r
toOutputType Phase = P.map unTagged
toOutputType Frequency = toFrequency >-> P.map unTagged

mixed :: MonadPrim m => Options -> Producer (TimeData Double) (Rand m) ()
mixed opts =
  zipSum $ catMaybes [ auxT whitePhase wpm
                     , auxT (flickerPhase (octaves size)) fpm
                     , auxF whiteFrequency wfm 
                     , auxF (flickerFrequency (octaves size)) ffm 
                     , auxF randomWalkFrequency rwfm 
                     , tourby
                     ]
  where auxT f g = f <$> view (mix . g) opts
        auxF f g = fmap (>-> toPhase) (auxT f g)
        size = view howMany opts
        tourby = (>-> toPhase)
             <$> uncurry tourbillonFrequency
             <$> view (mix . tourbillon) opts
        

