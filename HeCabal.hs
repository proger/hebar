{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module HeCabal where

import Data.List (intercalate)

import qualified Distribution.Verbosity as Cabal
import Distribution.PackageDescription (PackageDescription, GenericPackageDescription)
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Compiler as Compiler
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.ModuleName as Mod
import qualified Distribution.Package as Pkg
import qualified Language.Haskell.Extension as HS

import HebarLib hiding (intercalate)

type Command = String
type PkgToInfo = GenericPackageDescription -> Cabal.BuildInfo

(&) = flip (.)
x <-> y = x <> " " <> y

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

allExtensions = Cabal.defaultExtensions <> Cabal.oldExtensions <> Cabal.otherExtensions

-- extensions, options, cSources, cIncludeDirs, cIncludes, otherModules, ghcOptions :: Cabal.BuildInfo -> _
extensions         = allExtensions & ((\(HS.EnableExtension x) -> show x) <$>)
options            = Cabal.options
cSources           = Cabal.cSources
cIncludeDirs       = Cabal.includeDirs
cIncludes          = Cabal.includes
cppOptions         = Cabal.cppOptions
otherModules       = Cabal.otherModules & (mod2s <$>)
deps               = fmap (\(Pkg.Dependency p _) -> Pkg.unPackageName p) . Cabal.targetBuildDepends

ghcOptions :: Cabal.BuildInfo -> [String]
ghcOptions = options & mapMaybe isGhc & headOrEmpty
  where
    headOrEmpty [x] = x
    headOrEmpty _ = []

    isGhc (Compiler.GHC, x) = Just x
    isGhc _ = Nothing

libExportedModules :: GenericPackageDescription -> [String]
libExportedModules = unlib & Cabal.exposedModules & (mod2s <$>)

exeModules :: GenericPackageDescription -> [String]
exeModules = unexe & Cabal.exeModules & (mod2s <$>)

exeMain :: GenericPackageDescription -> FilePath
exeMain = unexe & Cabal.modulePath

exeName :: GenericPackageDescription -> String
exeName = unexe & Cabal.exeName


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
ghc p2i pkg modules = "ghc -v -clear-package-db --make" <-> defargs <-> opts <-> toArgs exts <-> toArgs modules <-> cpp <-> toArgs includes
  where
    defargs = "-O -j8 -static "
              <> "-I/usr/local/Cellar/ghc/7.8.3/lib/ghc-7.8.3/include "
              <> "-outputdir out -odir out -hidir out -stubdir out "
              <> "-i. -isrc "
              <> "-optP-D -optP'MIN_VERSION_base(a,b,c)=1' "
              <> "-optP-D -optP'MIN_VERSION_bytestring(a,b,c)=1' "

    exts = ["-X" <> e | e <- extensions bi]
    opts = toArgs $ ghcOptions bi
    cpp = toArgs $ cppOptions bi
    bi = p2i pkg
    includes = ["-I" <> i | i <- cIncludeDirs bi]

cc :: Cabal.BuildInfo -> FilePath -> Command
cc bi cfile = "ghc -c" <-> toArgs includes <-> defargs <-> cfile
  where
    defargs = "-fPIC "
              <> "-I/usr/local/Cellar/ghc/7.8.3/lib/ghc-7.8.3/include "
              <> "-odir out "
              <> "-optc-O2 "

    includes = ["-I" <> i | i <- cIncludeDirs bi]

staticlink :: GenericPackageDescription -> Command
staticlink pkg = "find out -name '*.o' | xargs ar -r -s out/" <> (pkgName pkg) <> ".a"


