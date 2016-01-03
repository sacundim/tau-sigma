-- | A test suite based on section 12.3 of W.J. Riley's
-- /Handbook of Frequency Stability Analysis/.
module TauSigma.Statistics.NBSSpec where

import Data.Tagged
import Data.Vector (Vector)
import qualified Data.Vector as V

import TauSigma.Types
import TauSigma.Statistics.Util (integrate)
import TauSigma.Statistics.Allan (adev, mdev, tdev)
import TauSigma.Statistics.Hadamard (hdev)
import TauSigma.Statistics.Total (totdev)
import TauSigma.Statistics.ComparisonTest

import Test.Hspec
  
spec :: Spec
spec = do
  describe "NBS Monograph 140, Annex 8.E" $ do
    describe "Overlapping Allan Deviation" $ do
      it "tau = 1" $ adev 1 1 `shouldBeAbout` 91.22945
      it "tau = 2" $ adev 1 2 `shouldBeAbout` 85.95287
  
    describe "Modified Allan Deviation" $ do
      it "tau = 1" $ mdev 1 1 `shouldBeAbout` 91.22945
      it "tau = 2" $ mdev 1 2 `shouldBeAbout` 74.78849

    describe "Time Deviation" $ do
      it "tau = 1" $ tdev 1 1 `shouldBeAbout` 52.67135
      it "tau = 2" $ tdev 1 2 `shouldBeAbout` 86.35831

    describe "Overlapping Hadamard Deviation" $ do
      it "tau = 1" $ hdev 1 1 `shouldBeAbout` 70.80607
      it "tau = 2" $ hdev 1 2 `shouldBeAbout` 85.61487
  
    describe "Total Deviation" $ do
      it "tau = 1" $ totdev 1 1 `shouldBeAbout` 91.22945
      it "tau = 2" $ totdev 1 2 `shouldBeAbout` 93.90379

    describe "Modified Total Deviation" $ do
      it "tau = 1" $ pendingWith (show 75.50203)
      it "tau = 2" $ pendingWith (show 75.83606)

    describe "Time Total Deviation" $ do
      it "tau = 1" $ pendingWith (show 43.59112)
      it "tau = 2" $ pendingWith (show 87.56794)
  where shouldBeAbout = comparison nbsData 5.0e-5


-- | NBS Monograph 140, Annex 8.E Test Data
nbsData :: Vector Double
nbsData = integrate (V.fromList (map untag raw))
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

