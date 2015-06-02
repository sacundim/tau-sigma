{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types shared by many modules in this application.
module TauSigma.Types
       ( Time(..)
       , Frequency(..)
       , TauSigma(..)
       ) where

import Control.Applicative
import Data.Csv
import qualified Data.Vector.Generic as G


-- | A newtype to represent time domain data values, to avoid mixing
-- them up with frequency domain data.
newtype Time a =
  Time { getTime :: a }
  deriving ( Eq
           , Ord
           , Show
           , Num
           , Fractional
           , Real
           , RealFloat
           , RealFrac
           , Floating
           , FromField
           , ToField
           )

-- | A newtype to represent frequency domain data values, to avoid
-- mixing them up with time domain data.
newtype Frequency a =
  Frequency { getFrequency :: a }
  deriving ( Eq
           , Ord
           , Show
           , Num
           , Fractional
           , Real
           , RealFloat
           , RealFrac
           , Floating
           , FromField
           , ToField
           )



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

