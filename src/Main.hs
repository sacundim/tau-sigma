{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Except

import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified TauSigma.TauSigma as TauSigma


main :: IO ()
main = do
  r <- runExceptT (TauSigma.main 86400)
  case r of
   Left err -> do
     hPutStrLn stderr err
     exitFailure
   Right () -> exitSuccess
