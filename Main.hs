{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (lift)
import Control.Monad.Except
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Csv

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

import Statistics.Allan


main :: IO ()
main = do result <- runExceptT main'
          case result of
           Left error -> putStrLn error
           Right () -> return ()

main' :: ExceptT String IO ()
main' = do input <- lift BS.getContents
           errors <- ExceptT $ return (readErrors input)
           lift $ BS.putStr (encode (adevs 86400 errors))

readErrors :: ByteString -> Either String (U.Vector Double)
readErrors input = fmap (V.convert . fmap fromOnly) $ decode NoHeader input
