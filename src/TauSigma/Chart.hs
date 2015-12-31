{-# LANGUAGE TemplateHaskell #-}

-- | Subcommand to chart phase/frequency data series.
module TauSigma.Chart
       ( Options
       , options
       , linear
       , loglog
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Csv (HasHeader(..), fromOnly)
import Data.Ord (comparing)
import Data.List(minimumBy)

import Graphics.Rendering.Chart.Easy hiding (label)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Utils (isValidNumber)

import Options.Applicative

import Pipes
import Pipes.ByteString (stdin)
import qualified Pipes.Prelude as P

import TauSigma.Types (TauSigma(..))
import TauSigma.Util.CSV

import Text.Printf


data Options
  = Options { _path :: FilePath
            , _label :: String
            }

$(makeLenses ''Options)


options :: Parser Options
options = Options
      <$> strOption
          ( long "out"
         <> short 'o'
         <> metavar "PATH"
         <> help "Path to write SVG file to"
          )
      <*> strOption
          ( long "label"
         <> short 'l'
         <> metavar "STRING"
         <> value "value"
         <> help "Label to use in graph legend"
          )


linear :: MonadIO m => Options -> ExceptT String m (PickFn ())
linear opts = do
  errors <- P.toListM (decode NoHeader stdin >-> P.map fromOnly)
  let charts = lineCharts [(view label opts, errors)]
  liftIO $ writeSVG opts charts

loglog
  :: MonadIO m =>
     Options
  -> ExceptT String m (PickFn (LayoutPick LogValue LogValue LogValue))
loglog opts = do
  points <- P.toListM (decodeByName stdin)
  liftIO $ writeSquareSVG opts (logLogChart (view label opts) points)


writeSVG :: Options -> Renderable a -> IO (PickFn a)
writeSVG opts = renderableToFile def (view path opts)

writeSquareSVG :: Options -> Renderable a -> IO (PickFn a)
writeSquareSVG opts =
  renderableToFile (set fo_size (800, 800) def) (view path opts)


lineCharts :: [(String, [Double])] -> Renderable ()
lineCharts sets =
  toRenderable $ execEC (mapM_ (plot . singleChart) sets)
  where singleChart :: (String, [Double]) ->  EC l (PlotLines Int Double)
        singleChart (name, points) = line name [(zip [1..] points)]


logLogChart
  :: String
  -> [TauSigma]
  -> Renderable (LayoutPick LogValue LogValue LogValue)
logLogChart name sigmas = layoutToRenderable layout
  where
    makePoint (TauSigma x y) = (LogValue (fromIntegral x), LogValue y)

    lines = plot_lines_values .~ [map makePoint sigmas]
          $ plot_lines_title .~ name
          $ def

    layout = layout_x_axis . laxis_generate .~ squareLogAxis def
           $ layout_x_axis . laxis_title .~ "tau"
           $ layout_y_axis . laxis_generate .~ squareLogAxis exponentialAxis
           $ layout_y_axis . laxis_title .~ "sigma"
           $ layout_plots .~ [ toPlot lines ]
           $ def

exponentialAxis :: LogAxisParams LogValue
exponentialAxis = LogAxisParams (\(LogValue a) -> printf "%0.1e" a)


----------------------------------------------------------------------------
----------------------------------------------------------------------------
--
-- The below is a tweaked copypaste from `haskell-charts` (the module
-- `Graphics.Rendering.Chart.Axis.Floating`)
--

-- | Truncate a value to the largest power of 10 that's less than or
-- equal to it.
magnitude :: (Floating a, RealFrac a) => a -> a
magnitude a = 10^^(floor (log10 a) :: Int)

magnitudeH :: (Floating a, RealFrac a) => a -> a
magnitudeH a = let m = magnitude a
               in if m == a then m else m * 10

-- | Like `autoScaledLogAxis`, but the axis ranges from the
-- `magnitude` of the smallest value to ten times that of the largest.
squareLogAxis :: RealFloat a => LogAxisParams a -> AxisFn a
squareLogAxis lap ps0 =
    makeAxis' (realToFrac . log) (realToFrac . exp)
              (_loga_labelf lap) (wrap rlabelvs, wrap rtickvs, wrap rgridvs)
        where
          ps        = filter (\x -> isValidNumber x && 0 < x) ps0
          (minV,maxV) = (magnitude (minimum ps), magnitudeH (maximum ps))
          wrap      = map fromRational
          range []  = (3,30)
          range _   | minV == maxV = (realToFrac $ minV/3, realToFrac $ maxV*3)
                    | otherwise    = (realToFrac $ minV,   realToFrac $ maxV)
          (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)

{-
 Rules: Do not subdivide between powers of 10 until all powers of 10
          get a major ticks.
        Do not subdivide between powers of ten as [1,2,4,6,8,10] when
          5 gets a major ticks
          (ie the major ticks need to be a subset of the minor tick)
-}
logTicks :: Range -> ([Rational],[Rational],[Rational])
logTicks (low,high) = (major,minor,major)
 where
  pf :: RealFrac a => a -> (Integer, a)
  pf = properFraction

  -- frac :: (RealFrac a, Integral b) => a -> (b, a)
  frac :: (RealFrac a) => a -> (Integer, a)
  frac x | 0 <= b    = (a,b)
         | otherwise = (a-1,b+1)
    where
      (a,b) = properFraction x

  ratio      = high/low
  lower a l  = let (i,r) = frac (log10 a) in
               maximum (1:filter (\x -> log10 (fromRational x) <= r) l)*10^^i
  upper a l  = let (i,r) = pf (log10 a) in
               minimum (10:filter (\x -> r <= log10 (fromRational x)) l)*10^^i
               
  powers           :: (Double,Double) -> [Rational] -> [Rational]
  powers (x,y) l    = [ a*10^^p | p <- [(floor (log10 x))..(ceiling (log10 y))] :: [Integer]
                                , a <- l ]
  midselection r l  = filter (inRange r l) (powers r l)
  inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)
  
  logRange = (log10 low, log10 high)
  
  roundPow x = 10^^(round x :: Integer)
  
  major | 17.5 < log10 ratio = map roundPow $
                               steps (min 5 (log10 ratio)) logRange
        | 12 < log10 ratio   = map roundPow $
                               steps (log10 ratio / 5) logRange
        | 6 < log10 ratio    = map roundPow $
                               steps (log10 ratio / 2) logRange
        | 3 < log10 ratio    = midselection (low,high) [1,10]
        | 20 < ratio         = midselection (low,high) [1,5,10]
        | 6 < ratio          = midselection (low,high) [1,2,4,6,8,10]
        | 3 < ratio          = midselection (low,high) [1..10]
        | otherwise          = steps 5 (low,high)

  (l',h')   = (minimum major, maximum major)
  (dl',dh') = (fromRational l', fromRational h')
  ratio' :: Double
  ratio' = fromRational (h'/l')
  filterX = filter (\x -> l'<=x && x <=h') . powers (dl',dh') 
  
  minor | 50 < log10 ratio' = map roundPow $
                              steps 50 (log10 dl', log10 dh')
        | 6 < log10 ratio'  = filterX [1,10]
        | 3 < log10 ratio'  = filterX [1,5,10]
        | 6 < ratio'        = filterX [1..10]
        | 3 < ratio'        = filterX [1,1.2..10]
        | otherwise         = steps 50 (dl', dh')

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps rs@(minV,maxV) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps rs
    min' :: Integer
    min' = floor   $ realToFrac minV / s
    max' = ceiling $ realToFrac maxV / s

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ log10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

log10 :: (Floating a) => a -> a
log10 = logBase 10

