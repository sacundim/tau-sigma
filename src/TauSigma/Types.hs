{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Types shared by many modules in this application.
module TauSigma.Types
       ( Domain(..)
       , TimeData
       , FreqData
       , Tagged(..)
       , TauSigma(..)
       ) where

import Control.Applicative
import Data.Csv
import Data.Tagged
import qualified Data.Vector.Generic as G

data Domain = Phase | Frequency deriving Read

-- | A tagged type to represent time domain data values, to avoid mixing
-- them up with frequency domain data.
type TimeData = Tagged Phase

-- | A newtype to represent frequency domain data values, to avoid
-- mixing them up with time domain data.
type FreqData = Tagged Frequency



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

