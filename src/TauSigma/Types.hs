{-# LANGUAGE OverloadedStrings #-}

module TauSigma.Types
       ( TauSigma(..)
       ) where

import Control.Applicative
import Data.Csv hiding (decode, encode)
import qualified Data.Vector.Generic as G

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

