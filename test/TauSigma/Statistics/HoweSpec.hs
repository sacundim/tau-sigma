
-- | Test suite for Theo1 statistics from this paper:
--
-- Howe, D.A.  2006.  "TheoH: a hybrid, high-confidence statistic that
-- improves on the Allan deviation."  Metrologia 43: S322-S331.
-- Available online: http://www.tf.nist.gov/timefreq/general/pdf/2109.pdf
--
module TauSigma.Statistics.HoweSpec where

import Data.Maybe (fromJust)
import Data.Tagged
import Data.Vector (Vector)
import qualified Data.Vector as V


import TauSigma.Types
import TauSigma.Statistics.Util (integrate)
import TauSigma.Statistics.Theo1 (theo1dev)
import TauSigma.Statistics.ComparisonTest

import Test.Hspec

spec :: Spec
spec = do
  describe "Howe 2006, Appendix A" $ do
    describe "Theo1 DEV" $ do
      it "m = 10" $ do
        (fromJust . theo1dev (86400 * 10^9) 10) `shouldBeAbout` 7.66e-15

  where shouldBeAbout = comparison howeData 1e-17


-- | Howe 2006, Appendix A test suite
howeData :: Vector Double
howeData = V.fromList (map unTagged raw)
  where raw :: [TimeData Double]
        raw = [ -2.15
              , -0.99
              , 1
              , 2.5
              , 0.65
              , -3.71
              , -3.3
              , 1.08
              , 0.5
              , 2.2
              , 4.68
              , 3.29
              ]

