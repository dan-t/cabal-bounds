{-# LANGUAGE PatternGuards #-}

module CabalBounds.Update
   ( update
   ) where

import qualified Distribution.PackageDescription as C
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import qualified Distribution.Simple.LocalBuildInfo as C
import qualified Distribution.Simple.PackageIndex as C
import qualified Distribution.InstalledPackageInfo as C
import Control.Lens
import CabalBounds.Bound (Bound(..))
import CabalBounds.Targets (Targets(..), dependenciesOf)
import CabalBounds.Dependencies (Dependencies, filterDependencies)
import CabalBounds.Lenses
import Data.List (sort, foldl', find)

type InstalledPackages = [(C.PackageName, C.Version)]

update :: Bound -> Targets -> Dependencies -> C.GenericPackageDescription -> C.LocalBuildInfo -> C.GenericPackageDescription
update bound AllTargets deps pkgDescrp buildInfo =
   pkgDescrp & dependenciesOfLib        . filterDeps %~ updateDep
             & dependenciesOfAllExes    . filterDeps %~ updateDep
             & dependenciesOfAllTests   . filterDeps %~ updateDep
             & dependenciesOfAllBenchms . filterDeps %~ updateDep
   where
      filterDeps = filterDependencies deps
      updateDep  = updateDependency bound (installedPackages buildInfo)


update bound (Targets targets) deps pkgDescrp buildInfo =
   foldl' updateTarget pkgDescrp targets
   where
      updateTarget pkgDescrp target =
         pkgDescrp & (dependenciesOf target) . filterDeps %~ updateDep

      filterDeps = filterDependencies deps
      updateDep  = updateDependency bound (installedPackages buildInfo)


updateDependency :: Bound -> InstalledPackages -> C.Dependency -> C.Dependency
updateDependency LowerBound instPkgs dep@(C.Dependency pkgName _)
   | Just (_, version) <- find ((== pkgName) . fst) instPkgs
   , Just intervals    <- versionIntervals version Nothing
   = C.Dependency pkgName (C.fromVersionIntervals intervals)

   | otherwise
   = dep

updateDependency UpperBound instPkgs dep@(C.Dependency pkgName versionRange)
   | not . C.isAnyVersion $ versionRange
   , Just (_, upperVersion)             <- find ((== pkgName) . fst) instPkgs
   , (C.LowerBound lowerVersion _, _):_ <- C.asVersionIntervals versionRange
   , Just intervals                     <- versionIntervals lowerVersion (Just $ nextMajorVersion upperVersion)
   = C.Dependency pkgName (C.fromVersionIntervals intervals)

   | otherwise
   = updateDependency BothBounds instPkgs dep

updateDependency BothBounds instPkgs dep@(C.Dependency pkgName _)
   | Just (_, version) <- find ((== pkgName) . fst) instPkgs
   , Just intervals    <- versionIntervals version (Just $ nextMajorVersion version)
   = C.Dependency pkgName (C.fromVersionIntervals intervals)

   | otherwise
   = dep


versionIntervals :: C.Version -> Maybe C.Version -> Maybe C.VersionIntervals
versionIntervals lowerVersion Nothing =
   C.mkVersionIntervals [(C.LowerBound lowerVersion C.InclusiveBound, C.NoUpperBound)]

versionIntervals lowerVersion (Just upperVersion) =
   C.mkVersionIntervals [(C.LowerBound lowerVersion C.InclusiveBound, C.UpperBound upperVersion C.ExclusiveBound)]


nextMajorVersion :: C.Version -> C.Version
nextMajorVersion version
   | (v1:v2:_) <- C.versionBranch version
   = C.Version {C.versionBranch = [v1, (v2 + 1)], C.versionTags = []}

   | (v1:_)    <- C.versionBranch version
   = C.Version {C.versionBranch = [v1, 1], C.versionTags = []}

   | otherwise
   = version


installedPackages :: C.LocalBuildInfo -> InstalledPackages
installedPackages = map (& _2 %~ newestVersion)
                    . filter ((not . null) . snd)
                    . C.allPackagesByName . C.installedPkgs
   where
      newestVersion :: [C.InstalledPackageInfo] -> C.Version
      newestVersion = last . sort . map (C.pkgVersion . C.sourcePackageId)
