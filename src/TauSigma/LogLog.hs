{-# LANGUAGE TemplateHaskell #-}

-- | Log/log graph renderer
module TauSigma.LogLog
       ( Options
       , options
       , main
       ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Control.Lens
import Control.Lens.TH

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


main :: MonadIO m => Options -> ExceptT String m (PickFn ())
main opts = do
  points <- P.toListM (decodeByName stdin)
  liftIO $ writeSVG (view path opts) [(view label opts, points)]

-- | Function to render multiple log-log plots on one chart.
logLogChart :: [(String, [TauSigma])] -> Renderable ()
logLogChart sets = toRenderable (execEC (mapM_ (plot . singleChart) sets))
    where singleChart (name, points) = line name [map makePoint points]
          makePoint (TauSigma x y) = (LogValue (fromIntegral x), LogValue y)

writeSVG :: FilePath -> [(String, [TauSigma])] -> IO (PickFn ())
writeSVG path = renderableToFile options path . logLogChart
  where options = FileOptions (640, 640) SVG mempty
        
