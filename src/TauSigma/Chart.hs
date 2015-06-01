{-# LANGUAGE TemplateHaskell #-}

-- | Subcommand to chart phase/frequency data series.
module TauSigma.Chart
       ( Options
       , options
       , linear
       , loglog
       ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Control.Lens
import Control.Lens.TH

import Data.Csv (HasHeader(..), fromOnly)
import Data.Monoid (mempty)

import Graphics.Rendering.Chart.Easy hiding (label)
import Graphics.Rendering.Chart.Backend.Diagrams

import Options.Applicative

import Pipes
import Pipes.ByteString (stdin)
import qualified Pipes.Prelude as P

import System.FilePath (FilePath, splitExtension)

import TauSigma.CSV
import TauSigma.Types (TauSigma(..))


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
         <> value "ADEV"
         <> help "Label to use in graph legend"
          )


linear :: MonadIO m => Options -> ExceptT String m (PickFn ())
linear opts = do
  errors <- P.toListM (decode NoHeader stdin >-> P.map fromOnly)
  let charts = lineCharts [(view label opts, errors)]
  liftIO $ writeSVG (view path opts) charts

loglog :: MonadIO m => Options -> ExceptT String m (PickFn ())
loglog opts = do
  points <- P.toListM (decodeByName stdin)
  liftIO $ writeSVG (view path opts) (logLogCharts [(view label opts, points)])

writeSVG :: FilePath -> Renderable () -> IO (PickFn ())
writeSVG path graph = renderableToFile def path graph
  where options = FileOptions (640, 640) SVG mempty


lineCharts :: [(String, [Double])] -> Renderable ()
lineCharts sets =
  toRenderable $ execEC (mapM_ (plot . singleChart) sets)
  where singleChart :: (String, [Double]) ->  EC l (PlotLines Int Double)
        singleChart (name, points) = line name [(zip [1..] points)]

logLogCharts :: [(String, [TauSigma])] -> Renderable ()
logLogCharts sets =
  toRenderable $ execEC (mapM_ (plot . singleChart) sets)
  where singleChart (name, points) = line name [map makePoint points]
        makePoint (TauSigma x y) = (LogValue (fromIntegral x), LogValue y)

