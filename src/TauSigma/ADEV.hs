{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TauSigma.ADEV
       ( Options
       , options
       , main
       ) where

import Control.Applicative

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Control.Lens (view)
import Control.Lens.TH

import Data.Csv (HasHeader(..), fromOnly, Header)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Options.Applicative hiding (header)

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Allan
import TauSigma.CSV
import TauSigma.Vector
import TauSigma.Types


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


main :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
main opts = do
  errors <- readVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ (each $ tauSigma opts errors)
          >-> encodeByName (V.fromList ["tau", "sigma"])
          >-> stdout

tauSigma :: Options -> U.Vector Double -> [TauSigma]
tauSigma opts xs = map toTauSigma (takeWhile below (adevs (view tau0 opts) xs))
  where toTauSigma (tau, sigma) = TauSigma tau sigma
        below (tau, _) = tau <= max
        max = fromMaybe def (view maxTau opts)
          where def = U.length xs - 1 `div` 5
