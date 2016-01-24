{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TauSigma.ADEV
       ( Statistic(..)
       , Options
       , options
       , main
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Control.Lens (view)
import Control.Lens.TH

import Data.Csv (HasHeader(..), fromOnly)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Options.Applicative hiding (header)

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Types (TauSigma(..))
import TauSigma.Statistics.Types (Tau0, Tau, Sigma)
import TauSigma.Statistics.Allan (adevs, mdevs, tdevs)
import TauSigma.Statistics.Hadamard (hdevs)
import TauSigma.Statistics.Total (totdevs)
import TauSigma.Statistics.Theo1 (theoBRdevs, theoHdevs)

import TauSigma.Util.CSV
import TauSigma.Util.Vector (drainToVector)


data Statistic = ADEV | MDEV | TDEV | HDEV | TOTDEV | TheoBR | TheoH

data Options
  = Options { _tau0 :: Tau0 Double
            , _maxTau :: Tau Double
            }

$(makeLenses ''Options)



options :: Parser Options
options = Options <$> tau0 <*> maxTau
  where f `with` xs = f (mconcat xs)
        tau0 = option auto
               `with` [ long "tau0"
                      , metavar "N"
                      , help "Base sampling interval"
                      ]
        maxTau = option auto
                 `with` [ long "max-tau"
                        , metavar "N"
                        , value 100
                        , help "Maximum multiple of tau0 to output."
                        ]


main :: (PrimMonad m, MonadIO m) =>
        Statistic
     -> Options
     -> ExceptT String m ()
main statistic opts = do
  errors <- drainToVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ each (dispatch statistic (view tau0 opts) errors)
          >-> P.takeWhile (\(tau, _) -> tau <= view maxTau opts)
          >-> P.map (uncurry TauSigma)
          >-> encodeByName (V.fromList ["tau", "sigma"])
          >-> stdout

dispatch
  :: Statistic
  -> Tau0 Double
  -> U.Vector Double
  -> [(Tau Double, Sigma Double)]
dispatch ADEV   = adevs 
dispatch MDEV   = mdevs
dispatch TDEV   = tdevs
dispatch HDEV   = hdevs
dispatch TOTDEV = totdevs
dispatch TheoBR = theoBRdevs
dispatch TheoH  = theoHdevs
