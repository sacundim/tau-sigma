{-# LANGUAGE OverloadedStrings #-}

module TauSigma.TauSigma
       ( TauSigma(..)
       , main
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Csv (HasHeader(..), fromOnly, Header)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Allan
import TauSigma.CSV
import TauSigma.Vector
import TauSigma.Types


main :: (PrimMonad m, MonadIO m) => Tau0 -> ExceptT String m ()
main tau0 = do
  errors <- readVector (decode NoHeader stdin >-> P.map fromOnly)
  runEffect $ (each $ tauSigma tau0 errors) >-> encodeByName header >-> stdout

header :: Header
header = V.fromList ["tau", "sigma"]

tauSigma :: Tau0 -> U.Vector Double -> [TauSigma]
tauSigma tau0 xs = map toTauSigma (adevs tau0 xs)
  where toTauSigma (tau, sigma) = TauSigma tau sigma

