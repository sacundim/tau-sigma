{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except

import Data.Monoid
import Data.Version (showVersion)
    
import Options.Applicative

import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified TauSigma.ADEV as ADEV
import qualified TauSigma.Chart as Chart
import qualified TauSigma.Convert as Convert
import qualified TauSigma.Noise as Noise

import Paths_tau_sigma (version)

main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> options) ( fullDesc )

main' opts = do
  r <- runExceptT (dispatch opts)
  case r of
   Left err -> do
     hPutStrLn stderr err
     exitFailure
   Right () -> exitSuccess


dispatch :: Options -> ExceptT String IO ()
dispatch (ADEV opts) = ADEV.main opts
dispatch (LogLog opts) = Chart.loglog opts >> return ()
dispatch (Chart opts) = Chart.linear opts >> return ()
dispatch (Convert opts) = Convert.main opts
dispatch (Noise opts) = lift (Noise.main opts)
dispatch Version = lift printVersion

printVersion :: IO ()
printVersion = putStrLn ("tau-sigma " ++ showVersion version)


options :: Parser Options
options =
  subparser $ mconcat
  [ command "adev"
      (info (ADEV <$> ADEV.options)
       (progDesc "Compute Allan deviation"))
  , command "loglog"
      (info (LogLog <$> Chart.options)
       (progDesc "Make a log/log tau/sigma graph"))
  , command "chart"
      (info (Chart <$> Chart.options)
       (progDesc "Make a linear graph"))
  , command "convert"
      (info (Convert <$> Convert.options)
       (progDesc "Make a linear graph"))
  , command "noise"
      (info (Noise <$> Noise.options)
       (progDesc "Generate spectral noises"))
  , command "version"
      (info (pure Version)
       (progDesc "Print version number"))
  ]

data Options
  = ADEV    ADEV.Options
  | LogLog  Chart.Options
  | Chart   Chart.Options
  | Convert Convert.Options
  | Noise   Noise.Options
  | Version
