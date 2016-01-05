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

import TauSigma.Types
import TauSigma.Statistics.Allan (adevs, mdevs, tdevs)
import TauSigma.Statistics.Hadamard (hdevs)
import TauSigma.Statistics.Total (totdevs)
import TauSigma.Statistics.Theo1 (theoBRdevs)
import TauSigma.Statistics.Util (Tau0)

import TauSigma.Util.CSV
import TauSigma.Util.DenseIntMap (UIntMap)
import qualified TauSigma.Util.DenseIntMap as IntMap
import TauSigma.Util.Vector (drainToVector)


data Statistic = ADEV | MDEV | TDEV | HDEV | TOTDEV | TheoBR

data Options
  = Options { _tau0 :: Tau0
            , _maxTau :: Int
            }

$(makeLenses ''Options)



options :: Parser Options
options = Options
      <$> option auto
          ( long "tau0"
         <> metavar "N"
         <> help "Base sampling interval (default 1)"
          )
      <*> option auto
          ( long "max-tau"
         <> metavar "N"
         <> value 100
         <> help "Maximum multiple of sampling intervals to output."
          )


main :: (PrimMonad m, MonadIO m) =>
        Statistic
     -> Options
     -> ExceptT String m ()
main statistic opts = do
  errors <- drainToVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ dispatch statistic (view tau0 opts) (view maxTau opts) errors
          >-> P.map (uncurry TauSigma)
          >-> encodeByName (V.fromList ["tau", "sigma"])
          >-> stdout

dispatch
  :: forall m. Monad m =>
     Statistic
  -> Tau0
  -> Int
  -> U.Vector Double
  -> Producer (Int, Double) m ()
dispatch ADEV tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (adevs tau0 xs)) 
dispatch MDEV tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (mdevs tau0 xs))
dispatch TDEV tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (tdevs tau0 xs))
dispatch HDEV tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (hdevs tau0 xs))
dispatch TOTDEV tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (totdevs tau0 xs))
dispatch TheoBR tau0 maxTau xs =
  IntMap.ieach (IntMap.takeBelow maxTau (theoBRdevs tau0 xs))

