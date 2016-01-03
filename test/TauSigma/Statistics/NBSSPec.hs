-- | A test suite based on section 12.3 of W.J. Riley's
-- /Handbook of Frequency Stability Analysis/.
module TauSigma.Statistics.RileySpec where

import Data.Tagged
import Data.Vector (Vector)
import qualified Data.Vector as V

import TauSigma.Types
import TauSigma.Statistics.Util (integrate)
import TauSigma.Statistics.Allan (adev)
import TauSigma.Statistics.Hadamard (hdev)
import TauSigma.Statistics.Total (totdev)

import Test.Hspec

spec :: Spec
spec = do
  describe "NBS Monograph 140, Annex 8.E Test Data" nbsSpec
    describe "Overlapping Allan Deviation" $ do
      it "tau = 1" $ adev 1 1 `shouldBeAbout` 91.22945
      it "tau = 2" $ adev 1 2 `shouldBeAbout` 85.95287
  
    describe "Overlapping Hadamard Deviation" $ do
      it "tau = 1" $ hdev 1 1 `shouldBeAbout` 70.80607
      it "tau = 2" $ hdev 1 2 `shouldBeAbout` 85.61487
  
    describe "Total Deviation" $ do
      it "tau = 1" $ totdev 1 1 `shouldBeAbout` 91.22945
      it "tau = 2" $ totdev 1 2 `shouldBeAbout` 93.90379

shouldBeAbout
  :: (Floating a, Ord a, Show a) =>
     (Vector (TimeData Double) -> a)
     -> a
     -> Expectation
stat `shouldBeAbout` expected =
  diff (stat nbsData) expected `shouldSatisfy` tolerance
  where diff :: Num a => a -> a -> a
        diff x y = abs (x - y)
        tolerance :: (Ord a, Floating a) => a -> Bool
        tolerance = (<=5.0e-5)

-- | NBS Monograph 140, Annex 8.E Test Data
nbsData :: Vector (TimeData Double)
nbsData = integrate (V.fromList (map retag raw))
  where raw :: [FreqData Double]
        raw = [ 8.920000000000000e+02
              , 8.090000000000000e+02
              , 8.230000000000000e+02
              , 7.980000000000000e+02
              , 6.710000000000000e+02
              , 6.440000000000000e+02
              , 8.830000000000000e+02
              , 9.030000000000000e+02
              , 6.770000000000000e+02
              ]

