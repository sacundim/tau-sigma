module TauSigma.Statistics.Types
       ( Tau0
       , Tau
       , Time
       , Sigma
       , TauSigma
       ) where

-- | Type synonym for values that represent the smallest sampling
-- interval in a data set.
type Tau0 a = a

-- | Type synonym for values that represent sampling intervals,
-- generally in result values.
type Tau a = a

-- | Type synonym for values that represent time-domain values.
type Time a = a

-- | Type synonym for values that represent statistical variances or
-- deviations, generally in result values.
type Sigma a = a


-- | A pair of a 'Tau' and a 'Sigma'.
type TauSigma a = (Tau a, Sigma a)
