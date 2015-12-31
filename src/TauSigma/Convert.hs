
-- | Utility to perform various kinds of conversions.
module TauSigma.Convert
       ( Options,
         options,
         main
       ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Csv (HasHeader(..), Only(..))

import Options.Applicative

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Types
import TauSigma.Util.CSV
import TauSigma.Util.Pipes

options :: Parser Options
options = Options
          <$> ( Unit
                <$> option auto
                    ( long "input-domain"
                   <> metavar "N"
                   <> value Frequency
                   <> help "Domain of input values. Default: Frequency."
                    )
                <*> option auto
                    ( long "input-denominator"
                   <> metavar "N"
                   <> value 86400
                   <> help "Denominator of input values. Default: 86,400"
                    )
              )
          <*> ( Unit
                <$> option auto
                    ( long "output-domain"
                   <> metavar "N"
                   <> value Phase
                   <> help "Domain of input values. Default: Phase."
                    )
                <*> option auto
                    ( long "output-denominator"
                   <> metavar "N"
                   <> value 86400
                   <> help "Denominator of output values. Default: 86,400"
                    )
              )

data Options =
  Options { _input  :: Unit
          , _output :: Unit
          }

-- | The units of the data points.  
data Unit =
  Unit {
    -- | The domain of the data points (time or frequency)
    _domain :: Domain

    -- | The denominator of the data points.  E.g., if the data is in
    -- seconds per day, this would be 86,400 (the number of seconds in
    -- a standard day).
    , _denominator   :: Double
    }

main :: MonadIO m => Options -> ExceptT String m ()
main (Options from to) =
  runEffect $ decode NoHeader stdin
          >-> P.map fromOnly
          >-> normalize from
          >-> denormalize to
          >-> P.map Only
          >-> encode
          >-> stdout

-- | Convert raw input data from the given unit to fractional phase data.
normalize :: Monad m => Unit -> Pipe Double (TimeData Double) m r
normalize (Unit Phase tau0) = P.map (Tagged . (/tau0))
normalize (Unit Frequency tau0) =
  integrate >-> normalize (Unit Phase tau0) 

-- | Convert fractional phase data to raw output data in the given unit.
denormalize :: Monad m => Unit -> Pipe (TimeData Double) Double m r
denormalize (Unit Phase tau0) = P.map ((*tau0) . unTagged)
denormalize (Unit Frequency tau0) =
  denormalize (Unit Phase tau0) >-> differentiate
