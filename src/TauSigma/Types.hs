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
       , Scale(..)
       , toScale
       , fromScale
       ) where

import Control.Applicative

import Data.Tagged
import Data.Semigroup (Semigroup(..), Min(..), Max(..))

import qualified Data.Vector.Generic as G

import Data.Csv

import TauSigma.Statistics.Types (Tau, Sigma)


data Domain = Phase | Frequency deriving Read

-- | A tagged type to represent time domain data values, to avoid mixing
-- them up with frequency domain data.
type TimeData = Tagged Phase

-- | A newtype to represent frequency domain data values, to avoid
-- mixing them up with time domain data.
type FreqData = Tagged Frequency



-- | A tau/sigma pair.
data TauSigma = TauSigma { tau :: !(Tau Double), sigma :: !(Sigma Double) }

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



-- | A 'Semigroup' for computing the scale of a non-empty set of
-- values.  By scale we mean the difference between the largest value
-- and the smallest.
data Scale a = Scale (Min a) (Max a)

toScale :: a -> Scale a
toScale a = Scale (Min a) (Max a)

fromScale :: Num a => Scale a -> a
fromScale (Scale (Min a) (Max b)) = b - a

instance Ord a => Semigroup (Scale a) where
  Scale a b <> Scale c d = Scale (a <> c) (b <> d)

