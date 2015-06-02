{-# LANGUAGE TemplateHaskell #-}

-- | Random noise generator supporting various spectral distributions
module TauSigma.Noise
       ( Options(..)
       , OutputType(..)
       , Mix(..)
       , options
       , main
       ) where

import Control.Applicative
import Control.Monad (forever, replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.Random (MonadRandom, Random, getRandomR)

import Data.Csv (Only(..))
import Data.Default
import Data.Maybe (catMaybes)
import Data.Monoid

import Control.Lens
import Control.Lens.TH

import Options.Applicative

import Pipes
import Pipes.ByteString (stdout)
import Pipes.Csv 
import qualified Pipes.Prelude as P

import TauSigma.Types

options :: Parser Options
options = Options
      <$> option auto
          ( long "length"
         <> short 'n'
         <> metavar "N"
         <> value 1000
         <> help "Generate N data points"
          )
      <*> flag Phase Freq
          ( long "frequency"
         <> help "Output as frequency data instead of phase"
          )
       <*> ( Mix
         <$> option (fmap Just auto)
             ( long "wpm"
            <> metavar "N"
            <> value Nothing
            <> help "White phase noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "fpm"
            <> metavar "N"
            <> value Nothing
            <> help "Flicker phase noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "wfm"
            <> metavar "N"
            <> value Nothing
            <> help "White frequency noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "ffm"
            <> metavar "N"
            <> value Nothing
            <> help "Flicker frequency noise intensity"
             )
         <*> option (fmap Just auto)
             ( long "rwfm"
            <> metavar "N"
            <> value Nothing
            <> help "Random walk frequency noise intensity"
             )
           )
         
data Options =
  Options { _howMany :: Int
          , _outputType :: OutputType
          , _mix :: Mix
          }

data OutputType = Phase | Freq

-- | The mix of noise types to generate.
data Mix =
  Mix { -- | White phase noise level.
        _wpm  :: Maybe Double
        -- | Flicker phase noise level.
      , _fpm  :: Maybe Double
        -- | White frequency noise level.
      , _wfm  :: Maybe Double
        -- | Flicker frequency noise level.
      , _ffm  :: Maybe Double
        -- | Random walk frequency noise level.
      , _rwfm :: Maybe Double
      }

instance Default Mix where
  def = Mix Nothing Nothing (Just 1.0) Nothing Nothing

$(makeLenses ''Options)
$(makeLenses ''Mix)



main :: (MonadRandom m, MonadIO m) => Options -> m ()
main opts =
  runEffect $ mixed (applyDefaults opts)
          >-> toOutputType (view outputType opts)
          >-> P.take (view howMany opts)
          >-> P.map Only
          >-> encode
          >-> stdout

applyDefaults :: Options -> Options
applyDefaults opts = over mix go opts
  where go (Mix Nothing Nothing Nothing Nothing Nothing) = def
        go other = other

--- | Handle the output type option.
toOutputType :: Monad m => OutputType -> Pipe (Time Double) Double m r
toOutputType Phase = P.map getTime
toOutputType Freq = toFrequency >-> P.map getFrequency


mixed :: MonadRandom m => Options -> Producer (Time Double) m ()
mixed opts =
  zipSum $ catMaybes [ auxT whitePhase wpm
                     , auxT (flickerPhase octaves) fpm
                     , auxF whiteFrequency wfm 
                     , auxF (flickerFrequency octaves) ffm 
                     , auxF randomWalkFrequency rwfm 
                     ]
  where auxT f g = f <$> view (mix . g) opts
        auxF f g = fmap (>-> toPhase) (auxT f g)
        octaves = floor $ logBase 2 (fromIntegral (view howMany opts))
        
whitePhase :: MonadRandom m => Double -> Producer (Time Double) m ()
whitePhase n = white n >-> P.map Time

flickerPhase :: MonadRandom m => Int -> Double -> Producer (Time Double) m ()
flickerPhase octaves n = flicker octaves n >-> P.map Time

whiteFrequency :: MonadRandom m => Double -> Producer (Frequency Double) m ()
whiteFrequency n = white n >-> P.map Frequency

flickerFrequency
  :: MonadRandom m => Int -> Double -> Producer (Frequency Double) m ()
flickerFrequency octaves n = flicker octaves n >-> P.map Frequency

randomWalkFrequency
  :: MonadRandom m => Double -> Producer (Frequency Double) m ()
randomWalkFrequency n = brown n >-> P.map Frequency


-- | White noise is just random values.
white :: MonadRandom m => Double -> Producer Double m ()
white n = randomR (-n, n)

-- | Brown noise is integrated white noise.
brown :: (MonadRandom m) => Double -> Producer Double m ()
brown n = white n >-> integrate

-- | Flicker noise has 1/f power density, i.e., inversely proportional
-- to the frequency.  
flicker :: (MonadRandom m) => Int -> Double -> Producer Double m ()
flicker octaves n = zipSum (map go [0..octaves])
  where go o = white n >-> hold o

-- | Hold a signal for @2^octave@ ticks.  (Yes, higher number = lower
-- octave.)
hold :: Monad m => Int -> Pipe a a m r
hold octave = forever $ do
  a <- await
  replicateM_ (2^octave) (yield a)


toPhase :: (Monad m, Num a) => Pipe (Frequency a) (Time a) m r
toPhase = P.map getFrequency >-> integrate >-> P.map Time

integrate :: (Monad m, Num a) => Pipe a a m r
integrate = P.scan (+) 0 id

toFrequency :: (Monad m, Num a) => Pipe (Time a) (Frequency a) m r
toFrequency = P.map getTime >-> differentiate >-> P.map Frequency

differentiate :: (Monad m, Num a) => Pipe a a m r
differentiate = await >>= differentiate'
  where
    differentiate' :: (Monad m, Num a) => a -> Pipe a a m r
    differentiate' prev = do
      cur <- await
      yield (cur - prev)
      differentiate' cur



-- | Ranged random 'Producer'.
randomR :: (MonadRandom m, Random a) => (a, a) -> Producer a m ()
randomR (lo, hi) = forever (lift (getRandomR (lo, hi)) >>= yield)


zipSum :: (Monad m, Num a) => [Producer a m ()] -> Producer a m ()
zipSum ps = foldr1 (P.zipWith (+)) ps
