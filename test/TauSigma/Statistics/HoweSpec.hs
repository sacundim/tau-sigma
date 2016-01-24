
-- | Test suite for Theo1 statistics from this paper:
--
-- Howe, D.A.  2006.  "TheoH: a hybrid, high-confidence statistic that
-- improves on the Allan deviation."  Metrologia 43: S322-S331.
-- Available online: http://www.tf.nist.gov/timefreq/general/pdf/2109.pdf
--
module TauSigma.Statistics.HoweSpec where

import Control.Monad (unless)

import Data.Maybe (fromJust)
import Data.Tagged
import Data.Vector (Vector)
import qualified Data.Vector as V

import Text.Printf (printf)

import TauSigma.Types
import TauSigma.Statistics.Util (integrate)
import TauSigma.Statistics.Theo1 (theo1dev)

import Test.Hspec


spec :: Spec
spec = do
  describe "Howe 2006, Appendix A" $ do
    describe "Theo1 DEV" $ do
      it "m = 10" $ do
        let (tau, sigma) = fromJust $ theo1dev 86400 10 howeData
        sigma `shouldBeAbout` (7.66e-15, 1e-17)

actual `shouldBeAbout` (expected, tolerance)  =
  unless (delta <= tolerance) (expectationFailure message)
  where
    delta  = abs (actual - expected)
    message =
      printf "expected = %f, actual = %f, delta = %f" expected actual delta



-- | Howe 2006, Appendix A test suite
howeData :: Vector Double
howeData = V.fromList (map (unTagged . (*1e-9)) raw)
  where
    -- These are given as nanoseconds, which we convert to seconds
    raw :: [TimeData Double]
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

