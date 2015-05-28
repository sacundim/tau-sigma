module TauSigma.LogLog
       ( logLogChart
       , writeSVG
       ) where

import Data.Monoid (mempty)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import System.FilePath (FilePath, splitExtension)
import TauSigma.Types (TauSigma(..))

-- | Function to render multiple log-log plots on one chart.
logLogChart :: [(String, [TauSigma])] -> Renderable ()
logLogChart sets = toRenderable (execEC (mapM_ (plot . singleChart) sets))
    where singleChart (name, points) = line name [map makePoint points]
          makePoint (TauSigma x y) = (LogValue (fromIntegral x), LogValue y)

writeSVG :: FilePath -> [(String, [TauSigma])] -> IO (PickFn ())
writeSVG path = renderableToFile options path . logLogChart
  where options = FileOptions (640, 640) SVG mempty
        
