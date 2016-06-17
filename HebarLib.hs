{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HebarLib (
  module Control.Applicative
, module Data.Monoid
, module Data.Maybe
, module Data.Either
, module System.Environment

, ByteString
, C.pack
, C.unpack
, C.intercalate
, C.putStrLn

, Url
, Pkg(..)
, topkg
) where

import Control.Applicative
import Data.Monoid
import Data.Either
import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import System.Environment

type Url = ByteString

data Pkg = Pkg
         { pkgname :: ByteString
         , pkgversion :: ByteString
         , pkgcabal :: Url
         , pkgtar :: Url
         } deriving (Show)

topkg' (x:_) | (C.head x) == '(' = Nothing
topkg' xs = Just $ Pkg{..}
  where
    pkgname = C.intercalate "-" $ init xs
    pkgversion = last xs
    pkgcabal =  hackage <> fqn <> "/" <> pkgname <> ".cabal"
    pkgtar = hackage <> fqn <> "/" <> fqn <> ".tar.gz"

    fqn = pkgname <> "-" <> pkgversion
    hackage = "http://hackage.haskell.org/package/"

topkg = topkg' . C.split '-'


