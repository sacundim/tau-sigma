{-# LANGUAGE OverloadedStrings #-}

module TauSigma.TauSigma
       ( TauSigma(..)
       , main
       ) where

import Control.Applicative
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)
import Data.Csv hiding (decode, encode)
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

import Pipes
import Pipes.ByteString (stdin, stdout)
import qualified Pipes.Prelude as P

import TauSigma.Allan
import TauSigma.CSV
import TauSigma.Vector

-- | A tau/sigma pair.
data TauSigma = TauSigma { tau :: !Int, sigma :: !Double }

instance FromRecord TauSigma where
  parseRecord v
    | G.length v == 2 = TauSigma <$> v.! 0 <*> v .! 1
    | otherwise = empty;

instance FromNamedRecord TauSigma where
     parseNamedRecord m = TauSigma <$> m .: "tau" <*> m .: "sigma"
                          
instance ToRecord TauSigma where
  toRecord (TauSigma tau sigma) =
    record [toField tau, toField sigma]

instance ToNamedRecord TauSigma where
  toNamedRecord (TauSigma tau sigma) =
    namedRecord ["tau" .= tau, "sigma" .= sigma]


main :: (PrimMonad m, MonadIO m) => Tau0 -> ExceptT String m ()
main tau0 = do
  errors <- readVector ((decode NoHeader stdin) >-> P.map fromOnly)
  runEffect $ (each $ tauSigma tau0 errors) >-> encode >-> stdout

tauSigma :: Tau0 -> Vector Double -> [TauSigma]
tauSigma tau0 xs = map toTauSigma (adevs tau0 xs)
  where toTauSigma (tau, sigma) = TauSigma tau sigma
