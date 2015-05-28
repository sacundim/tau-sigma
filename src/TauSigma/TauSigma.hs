module TauSigma.TauSigma
       ( TauSigma(..)
       , main
       ) where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)
import Data.Csv (HasHeader(..), fromOnly)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Allan
import TauSigma.CSV
import TauSigma.Vector
import TauSigma.Types
import TauSigma.LogLog


main :: (PrimMonad m, MonadIO m) => Tau0 -> FilePath -> ExceptT String m ()
main tau0 path = do
  errors <- readVector ((decode NoHeader stdin) >-> P.map fromOnly)
  runEffect $ (each $ tauSigma tau0 errors) >-> encode >-> stdout
  liftIO $ writeSVG path [("ADEV", tauSigma 86400 errors)]
  return ()

tauSigma :: Tau0 -> Vector Double -> [TauSigma]
tauSigma tau0 xs = map toTauSigma (adevs tau0 xs)
  where toTauSigma (tau, sigma) = TauSigma tau sigma

