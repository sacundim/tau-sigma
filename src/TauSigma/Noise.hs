{-# LANGUAGE TemplateHaskell #-}

-- | Random noise generator supporting various spectral distributions
module TauSigma.Noise
       ( Options(..)
       , Domain(..)
       , Mix(..)
       , options
       , main
       ) where

import Control.Lens
import Control.Monad.Primitive (PrimMonad)

import Data.Functor.Compose (Compose(..))

import Data.Csv (Only(..))
import Data.Default
import Data.Maybe (catMaybes)

import Data.Random (RVarT, runRVarT)
import System.Random.MWC (createSystemRandom)
    
import Options.Applicative

import Pipes
import Pipes.ByteString (stdout)
import Pipes.Csv 
import qualified Pipes.Prelude as P

import TauSigma.Types
import TauSigma.Util.Pipes.Noise

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
                            , help "Simulate a tourbillon with period of N."
                            ]
            level = Compose $ option (fmap Just auto)
                    `with` [ long "tourbillon-level"
                           , metavar "N"
                           , value (Just 1.0)
                            , help $ "Tourbillon effect intensity level.  "
                                  ++ "Default: 1.0"
                           ]
                  

main :: Options -> IO ()
main opts = runWithSystemRandom main'
  where main' :: RVarT IO ()
        main' = runEffect $ mixed (applyDefaults opts)
            >-> toOutputType (view outputType opts)
            >-> P.take (view howMany opts)
            >-> P.map Only
            >-> encode
            >-> stdout

runWithSystemRandom :: RVarT IO a -> IO a
runWithSystemRandom ma = runRVarT ma =<< createSystemRandom

applyDefaults :: Options -> Options
applyDefaults opts = over mix go opts
  where go (Mix Nothing Nothing Nothing Nothing Nothing Nothing) = def
        go other = other

--- | Handle the output type option.
toOutputType :: Monad m => Domain -> Pipe (TimeData Double) Double m r
toOutputType Phase = P.map unTagged
toOutputType Frequency = toFrequency >-> P.map unTagged

mixed :: PrimMonad m => Options -> Producer (TimeData Double) (RVarT m) ()
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
        

