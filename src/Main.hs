{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except

import Data.Monoid

import Options.Applicative

import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified TauSigma.TauSigma as TauSigma
import qualified TauSigma.LogLog as LogLog
import qualified TauSigma.Noise as Noise


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
dispatch (ADEV opts) = TauSigma.main opts
dispatch (LogLog opts) = LogLog.main opts >> return ()
dispatch (Noise opts) = lift (Noise.main opts)


options :: Parser Options
options =
  subparser $ mconcat
  [ command "adev"
      (info (ADEV <$> TauSigma.options)
       (progDesc "Compute Allan deviation"))
  , command "loglog"
      (info (LogLog <$> LogLog.options)
       (progDesc "Make a log/log tau/sigma graph"))
  , command "noise"
      (info (Noise <$> Noise.options)
       (progDesc "Generate spectral noises"))
  ]

data Options
  = ADEV TauSigma.Options
  | LogLog LogLog.Options
  | Noise Noise.Options
