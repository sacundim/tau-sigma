{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TauSigma.ADEV
       ( Options
       , options
       , adev
       , mdev
       , tdev
       , hdev
       , totdev
       , theoBRdev
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

import TauSigma.Types
import TauSigma.Statistics.Allan (adevs, mdevs, tdevs)
import TauSigma.Statistics.Hadamard (hdevs)
import TauSigma.Statistics.Total (totdevs)
import TauSigma.Statistics.Theo1 (theoBRdevs)
import TauSigma.Statistics.Util (Tau0)

import TauSigma.Util.CSV
import TauSigma.Util.DenseIntMap (UIntMap)
import qualified TauSigma.Util.DenseIntMap as IntMap
import TauSigma.Util.Vector


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
adev = run adevs

mdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
mdev = run mdevs

tdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
tdev = run tdevs

hdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
hdev = run hdevs

totdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
totdev = run totdevs

theoBRdev :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
theoBRdev = run theoBRdevs

type Statistic = Tau0 -> U.Vector Double -> UIntMap Double

run :: (PrimMonad m, MonadIO m) =>
       Statistic
    -> Options
    -> ExceptT String m ()
run statistic opts = do
  errors <- drainToVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ tauSigma opts statistic errors
          >-> encodeByName (V.fromList ["tau", "sigma"])
          >-> stdout


tauSigma
  :: Monad m =>
     Options
  -> Statistic
  -> U.Vector Double
  -> Producer TauSigma m ()
tauSigma opts statistic xs = each (IntMap.toSparseList result)
                         >-> P.takeWhile (below (view maxTau opts))
                         >-> P.map toTauSigma
  where result = statistic (view tau0 opts) xs
        below Nothing (tau, _) = tau <= 100
        below (Just max) (tau, _) = tau <= max
        toTauSigma (tau, sigma) = TauSigma tau sigma
  
