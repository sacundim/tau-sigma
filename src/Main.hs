{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except

import Data.Default (def)

import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified TauSigma.TauSigma as TauSigma
import qualified TauSigma.LogLog as LogLog
import qualified TauSigma.Noise as Noise


main :: IO ()
main = do
  args <- getArgs
  r <- runExceptT (dispatch args)
  case r of
   Left err -> do
     hPutStrLn stderr err
     exitFailure
   Right () -> exitSuccess

dispatch :: [String] -> ExceptT String IO ()
dispatch ["adev", tau0] = TauSigma.main (read tau0)
dispatch ["graph", path] = LogLog.main path >> return ()
dispatch ["noise"] = lift (Noise.main def)
dispatch other = throwE ("Illegal arguments: " ++ show other)
