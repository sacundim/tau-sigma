{-# LANGUAGE ScopedTypeVariables #-}

module TauSigma.Util.AllanSpec where

import Data.Vector (Vector)
import qualified Data.Vector as V

import TauSigma.Util.Allan

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "differences/integrate" $ do
    it "integrate is left inverse of differences" $ property $
      \(xs :: Vector Int) -> differences (integrate xs) == xs



-----------------------------------------------------------------

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap V.fromList arbitrary
