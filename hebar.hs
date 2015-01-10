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

import qualified Distribution.Verbosity as Cabal
import Distribution.PackageDescription (PackageDescription, GenericPackageDescription)
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Compiler as Compiler
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.ModuleName as Mod
import qualified Distribution.Package as Pkg
import qualified Language.Haskell.Extension as HS

data Pkg = Pkg { pkgname :: ByteString, pkgversion :: ByteString }
         deriving (Show)

version (Pkg n v) = n <> "-" <> v

toCabal pkg@(Pkg n v) = "http://hackage.haskell.org/package/" <> (version pkg) <> "/" <> n <> ".cabal"
toTarGz pkg = "http://hackage.haskell.org/package/" <> (version pkg) <> "/" <> (version pkg) <> ".tar.gz"

topkg (x:_) | (C.head x) == '(' = Nothing
topkg xs = Just $ Pkg name version
  where
    name = C.intercalate "-" $ init xs
    version = last xs

f = topkg . C.split '-'

resolve = do
  stuff <- C.lines <$> C.getContents
  forM_ (fmap toCabal $ catMaybes $ f <$> stuff) $ \link -> do
    C.putStrLn link

type Command = String
type PkgToInfo = GenericPackageDescription -> Cabal.BuildInfo

(&) = flip (.)

mod2s = intercalate "." . Mod.components

uncabal :: FilePath -> IO GenericPackageDescription
uncabal = Cabal.readPackageDescription Cabal.normal

pkgName :: GenericPackageDescription -> String
pkgName = Pkg.unPackageName . Pkg.pkgName . Cabal.package . Cabal.packageDescription 

unlib :: GenericPackageDescription -> Cabal.Library
unlib = Cabal.condLibrary & fromJust & Cabal.condTreeData

unexe :: GenericPackageDescription -> Cabal.Executable
unexe = Cabal.condExecutables & head & snd & Cabal.condTreeData

libBI :: PkgToInfo
libBI = unlib & Cabal.libBuildInfo

exeBI :: PkgToInfo
exeBI = unexe & Cabal.buildInfo

-- extensions, options, cSources, cIncludeDirs, cIncludes, otherModules :: Cabal.BuildInfo -> _
extensions         = Cabal.otherExtensions & ((\(HS.EnableExtension x) -> show x) <$>)
options            = Cabal.options
cSources           = Cabal.cSources
cIncludeDirs       = Cabal.includeDirs
cIncludes          = Cabal.includes
otherModules       = Cabal.otherModules & (mod2s <$>)

libExportedModules :: GenericPackageDescription -> [String]
libExportedModules = unlib & Cabal.exposedModules & (mod2s <$>)

exeModules :: GenericPackageDescription -> [String]
exeModules = unexe & Cabal.exeModules & (mod2s <$>)

exeMain :: GenericPackageDescription -> FilePath
exeMain = unexe & Cabal.modulePath

exeName :: GenericPackageDescription -> String
exeName = unexe & Cabal.exeName

unGhcOptions = options & mapMaybe isGhc & headOrEmpty
  where
    headOrEmpty [x] = x
    headOrEmpty _ = []

    isGhc (Compiler.GHC, x) = Just x
    isGhc _ = Nothing

toArgs = intercalate " "

ghcExe :: GenericPackageDescription -> Command
ghcExe pkg = base <> " -o out/bin/" <> (pkgName pkg)
    where
      base = ghc exeBI pkg (exeModules pkg <> ["src/" <> exeMain pkg])

ghcLib :: GenericPackageDescription -> Command
ghcLib pkg = ghc libBI pkg (mods <> other)
  where
    mods = libExportedModules pkg
    other = otherModules (libBI pkg)

ghc :: PkgToInfo -> GenericPackageDescription -> [String] -> Command
ghc p2i pkg modules = "ghc --make " <> defargs <> opts <> " " <> toArgs exts <> " " <> toArgs modules
  where
    defargs = "-O -j8 -static "
              <> "-outputdir out -odir out -hidir out -stubdir out "
              <> "-i. -isrc "
              <> "-optP-D -optP'MIN_VERSION_base(a,b,c)=1' "

    exts = ["-X" <> e | e <- extensions bi]
    opts = toArgs $ unGhcOptions bi
    bi = p2i pkg

cc :: Cabal.BuildInfo -> FilePath -> Command
cc pkg cfile = "ghc -c " <> toArgs includes <> " " <> defargs <> cfile
  where
    defargs = "-fPIC "
              <> "-odir out "
              <> "-optc-O2 "

    includes = ["-I" <> i | i <- cIncludeDirs pkg]

staticlink :: GenericPackageDescription -> Command
staticlink pkg = "find out -name '*.o' | xargs ar -r -s out/" <> (pkgName pkg) <> ".a"


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
  --printLib file
  printExe file
