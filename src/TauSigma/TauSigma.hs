{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TauSigma.TauSigma
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


data Options = Options { _tau0 :: Tau0 }

$(makeLenses ''Options)

options :: Parser Options
options = Options
      <$> option auto
          ( long "tau0"
         <> metavar "N"
         <> help "Sampling interval (default 1)"
          )


main :: (PrimMonad m, MonadIO m) => Options -> ExceptT String m ()
main opts = do
  let 
  errors <- readVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ (each $ tauSigma (view tau0 opts) errors)
          >-> encodeByName header
          >-> stdout

header :: Header
header = V.fromList ["tau", "sigma"]

tauSigma :: Tau0 -> U.Vector Double -> [TauSigma]
tauSigma tau0 xs = map toTauSigma (adevs tau0 xs)
  where toTauSigma (tau, sigma) = TauSigma tau sigma

