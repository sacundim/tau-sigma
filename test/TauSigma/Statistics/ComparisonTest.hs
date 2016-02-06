module TauSigma.Statistics.ComparisonTest
       ( comparison
       ) where

import Control.Monad (unless)

import Data.Vector (Vector)

import Text.Printf (printf)

import Test.Hspec (Expectation, expectationFailure)


comparison
  :: Vector Double
  -> Double
  -> (Vector Double -> Double)
  -> Double
  -> Expectation
comparison dataSet tolerance = shouldBeAbout
  where
    shouldBeAbout
      :: (Vector Double -> Double)
      -> Double
      -> Expectation
    stat `shouldBeAbout` expected =
      unless (delta <= tolerance) (expectationFailure message)
      where
        actual = stat dataSet
        delta  = abs (actual - expected)
        message =
          printf "expected = %f, actual = %f, delta = %f"
                 expected actual delta
