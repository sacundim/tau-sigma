{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TauSigma.ADEV
       ( Options
       , options
       , adev
       , theoBRdev
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Control.Lens (view)
import Control.Lens.TH

import Data.Csv (HasHeader(..), fromOnly)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Options.Applicative hiding (header)

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Types
import TauSigma.Statistics.Allan (adevs)
import TauSigma.Statistics.Theo1 (theoBRdevs)
import TauSigma.Statistics.Util (Tau0)
import TauSigma.Util.CSV
import TauSigma.Util.Vector


type Statistic = U.Vector Double -> IntMap Double
  
data Options
  = Options { _tau0 :: Tau0
            , _maxTau :: Maybe Int
            }

$(makeLenses ''Options)

options :: Parser Options
options = Options
      <$> option auto
          ( long "tau0"
         <> metavar "N"
         <> help "Base sampling interval (default 1)"
          )
      <*> option (fmap Just auto)
          ( long "max-tau"
         <> metavar "N"
         <> value Nothing
         <> help "Maximum multiple of sampling intervals to output."
          )


adev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
adev opts = run opts (adevs (view tau0 opts))

theoBRdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
theoBRdev opts = run opts (theoBRdevs (view tau0 opts))

run :: (PrimMonad m, MonadIO m) => Options -> Statistic -> ExceptT String m ()
run opts statistic = do
  errors <- drainToVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ (each $ tauSigma opts statistic errors)
          >-> encodeByName (V.fromList ["tau", "sigma"])
          >-> stdout

tauSigma :: Options -> Statistic -> U.Vector Double -> [TauSigma]
tauSigma opts statistic xs = map toTauSigma truncated
  where
    -- Note: the `TauSigma` constructor is strict in all its fields
    toTauSigma (tau, sigma) = TauSigma tau sigma
    truncated = takeWhile below (IntMap.toAscList (statistic xs))
      where below (tau, _) = tau <= max
            max = fromMaybe def (view maxTau opts)
              where def = (U.length xs - 1) `div` 5
