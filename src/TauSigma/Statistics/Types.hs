module TauSigma.Statistics.Types
       ( Tau0
       , Tau
       , Time
       , Sigma
       , TauSigma
       , OneTau
       , AllTau
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


-- | The common type that we use for most of the statistical functions
-- | that produce a result at only one tau.
type OneTau v = Tau0 Double -> Int -> v (Time Double) -> Sigma Double

-- | The common type that we use for most of the statistical functions
-- | that produce a result at all taus.
type AllTau v = Tau0 Double -> v (Time Double) -> [TauSigma Double]
