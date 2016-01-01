{-# LANGUAGE ScopedTypeVariables #-}

module TauSigma.Util.AllanSpec where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Primitive.Class (MonadPrim)

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as V

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC.Monad (Rand, runWithCreate)

import TauSigma.Util.Allan (adevs)
import TauSigma.Util.Pipes.Noise (Rand, white, integrate)

import Test.Hspec

spec :: Spec
spec = do
  describe "wfm slope" $ do
    it "The slope of white frequency noise should be about -0.5" $ do
      wfm <- runWithCreate (makeNoise (white 1.0 >-> integrate) 100000)
      let graph = adevs 1 wfm
      let failures = badSlopes (-0.5) 3.0e-2 graph
      take 10 failures `shouldBe` []
      

  describe "ffm slope" $ do
    it "The slope of flicker frequency noise should be about 0.0" $ do
      pending

  describe "rwfm slope" $ do
    it "The slope of random walk frequency noise should be about 0.5" $ do
      pending

type Point = (Int, Double)
type Slope = Double
type Error = Double

badSlopes :: Slope -> Error -> IntMap Double -> [(Point, Point, Slope, Error)]
badSlopes standard tolerance graph =
  [ (p0, p1, slope, err)
  | let points = take 10 (IntMap.toList graph)
  , (p0@(x0, y0), p1@(x1, y1)) <- withSuccessors (,) points
  , let slope = logSlope (fromIntegral x0, y0) (fromIntegral x1, y1)
  , let err = abs (slope - standard)
  , err > tolerance
  ]

withSuccessors :: (a -> a -> r) -> [a] -> [r]
withSuccessors _ [] = []
withSuccessors f (a:as) = map (f a) as ++ withSuccessors f as

logSlope :: (Double, Double) -> (Double, Double) -> Double
logSlope (x0,y0) (x1,y1) = slope (log x0, log y0) (log x1, log y1)
  where
    slope :: (Double, Double) -> (Double, Double) -> Double
    slope (x0,y0) (x1,y1) = (y1 - y0) / (x1 - x0)
                

makeNoise
  :: Monad m =>
     Producer Double (Rand m) ()
  -> Int
  -> Rand m (V.Vector Double)
makeNoise source size = toVector (source >-> P.take size) 

toVector :: Monad m => Producer a m () -> m (V.Vector a)
toVector = fmap V.fromList . P.toListM 
