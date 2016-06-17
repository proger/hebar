{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Char (toUpper, isDigit)
import Data.Monoid
import Data.List (intercalate)

import HeCabal

printLib file = do
  pkg <- uncabal file
  putStrLn $ ghcLib pkg
  forM_ (cSources . libBI $ pkg) $ \src -> do
    putStrLn $ cc (libBI pkg) src
  putStrLn $ staticlink pkg

printExe file = do
  pkg <- uncabal file
  putStrLn $ ghcExe pkg

main = do
  file <- getArgs >>= \case
                      (f:_) -> return f
                      _ -> fail "usage: haresolve file.cabal"
  printLib file
  --printExe file
