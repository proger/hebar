{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (map)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.FilePath ((</>))
import System.Process (callProcess, callCommand)

import qualified Distribution.PackageDescription as Cabal

import HebarLib
import HeCabal

data Actions = Cloning Origin
             | Get String
             | Guess String
             | Need (Set String)
             | Missing [String]
               deriving (Show)

data Origin = Origin String Url String
              deriving (Show)

set = Set.fromList
map = Map.fromList

lookupEither k m =
  case Map.lookup k m of
    Just x -> Right x
    Nothing -> Left k

-- ghc-pkg list --no-user-package-db
ignores = set
  [ "Cabal"     , "array"      , "base"           , "bin-package-db"
  , "binary"    , "bytestring" , "containers"     , "deepseq"
  , "directory" , "filepath"   , "ghc-prim"       , "haskeline"
  , "hoopl"     , "hpc"        , "integer-gmp"    , "old-locale"
  , "old-time"  , "pretty"     , "process"        , "rts"        , "template-haskell"
  , "terminfo"  , "time"       , "transformers"   , "unix"       , "xhtml"
  ]

known = map'
  [ Origin "mtl" "http://github.com/ekmett/mtl" "v2.2.1"
  , Origin "configurator" "http://github.com/bos/configurator" "0.3.0.0"
  , Origin "stm" "http://git.haskell.org/packages/stm.git" "stm-2.4.4-release"
  , Origin "unordered-containers" "https://github.com/tibbe/unordered-containers" "v0.2.5.0"
  , Origin "text" "https://github.com/bos/text" "1.2.0.4"
  , Origin "attoparsec" "https://github.com/bos/attoparsec" "0.12.1.2"
  , Origin "unix-compat" "http://github.com/jystic/unix-compat" "0.4.1.4"
  , Origin "hashable" "http://github.com/tibbe/hashable" "v1.2.3.1"
  , Origin "scientific" "https://github.com/basvandijk/scientific" "0.3.3.5"
  ]
  where
    map' = map . (fmap tu)
    tu o@(Origin k _ _) = (k, o)

needPackages :: Set String -> Cabal.BuildInfo -> Set String
needPackages have bi = Set.difference pkdeps (Set.union have ignores)
  where
    pkdeps = set (deps bi)

gitGuess :: String -> IO ()
gitGuess p = do
  print (Guess p)
  callCommand ("cabal info " <> p <> " | awk -F': ' '/Version/ {print $0 > \"/dev/stderr\"} /git/ && !n {print $2; n=1}' | xargs -tn1 git ls-remote | grep refs.tags | tail -n1")

clone :: Origin -> IO Cabal.GenericPackageDescription
clone o@(Origin name git version) = do
  print (Cloning o)
  callProcess "/Users/vladki/src/hebar/cachedgit" [name, unpack git, version]
  uncabal (name </> name <> ".cabal")

get :: PkgToInfo -> Cabal.GenericPackageDescription -> IO ()
get p2i pkg = do
  print (Get (pkgName pkg))
  let needed = needPackages Set.empty (p2i pkg)
  print (Need needed)
  let origins = (flip lookupEither known) <$> (Set.toList needed)
  case partitionEithers origins of
      ([], rights) -> do
        pkgs <- mapM clone rights
        mapM_ (get libBI) pkgs
      (lefts, _) -> do
        print (Missing lefts)
        mapM_ gitGuess lefts
        error "missing some origins"

main = do
  file:_ <- getArgs
  pkg <- uncabal file
  get exeBI pkg
