{-# LANGUAGE PatternGuards #-}

module CabalBounds.Update
   ( update
   ) where

import qualified Distribution.PackageDescription as D
import qualified Distribution.Package as P
import qualified Distribution.Version as V
import qualified Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.PackageIndex as PX
import qualified Distribution.InstalledPackageInfo as PI
import Control.Lens
import CabalBounds.Bound (UpdateBound(..))
import CabalBounds.Targets (Targets(..), dependenciesOf)
import CabalBounds.Dependencies (Dependencies, filterDependencies)
import CabalBounds.VersionComp (VersionComp(..), defaultLowerComp)
import CabalBounds.Lenses
import Data.List (sort, foldl', find)

type InstalledPackages = [(P.PackageName, V.Version)]

update :: UpdateBound -> Targets -> Dependencies -> D.GenericPackageDescription -> L.LocalBuildInfo -> D.GenericPackageDescription
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


updateDependency :: UpdateBound -> InstalledPackages -> P.Dependency -> P.Dependency
updateDependency (UpdateLower comp) instPkgs dep@(P.Dependency pkgName _)
   | Just (_, version) <- find ((== pkgName) . fst) instPkgs
   , Just intervals    <- versionIntervals (versionComp comp version) Nothing
   = P.Dependency pkgName (V.fromVersionIntervals intervals)

   | otherwise
   = dep

updateDependency (UpdateUpper comp) instPkgs dep@(P.Dependency pkgName versionRange)
   | not . V.isAnyVersion $ versionRange
   , Just (_, upperVersion)             <- find ((== pkgName) . fst) instPkgs
   , (V.LowerBound lowerVersion _, _):_ <- V.asVersionIntervals versionRange
   , Just intervals                     <- versionIntervals lowerVersion (Just $ nextVersion $ versionComp comp upperVersion)
   = P.Dependency pkgName (V.fromVersionIntervals intervals)

   | otherwise
   = updateDependency (UpdateBoth defaultLowerComp comp) instPkgs dep

updateDependency (UpdateBoth lowerComp upperComp) instPkgs dep@(P.Dependency pkgName _)
   | Just (_, version) <- find ((== pkgName) . fst) instPkgs
   , Just intervals    <- versionIntervals (versionComp lowerComp version) (Just $ nextVersion $ versionComp upperComp version)
   = P.Dependency pkgName (V.fromVersionIntervals intervals)

   | otherwise
   = dep


versionIntervals :: V.Version -> Maybe V.Version -> Maybe V.VersionIntervals
versionIntervals lowerVersion Nothing =
   V.mkVersionIntervals [(V.LowerBound lowerVersion V.InclusiveBound, V.NoUpperBound)]

versionIntervals lowerVersion (Just upperVersion) =
   V.mkVersionIntervals [(V.LowerBound lowerVersion V.InclusiveBound, V.UpperBound upperVersion V.ExclusiveBound)]


versionComp :: VersionComp -> V.Version -> V.Version
versionComp Major1 version =
   version & vbranch %~ take 1
           & vtags   .~ []

versionComp Major2 version =
   version & vbranch %~ take 2
           & vtags   .~ []

versionComp Minor version =
   version & vtags .~ []


nextVersion :: V.Version -> V.Version
nextVersion version =
   version & vbranch %~ increaseLast
   where
      increaseLast = reverse . (& ix 0 %~ (+ 1)) . reverse


installedPackages :: L.LocalBuildInfo -> InstalledPackages
installedPackages = map (& _2 %~ newestVersion)
                    . filter ((not . null) . snd)
                    . PX.allPackagesByName . L.installedPkgs
   where
      newestVersion :: [PI.InstalledPackageInfo] -> V.Version
      newestVersion = last . sort . map (P.pkgVersion . PI.sourcePackageId)
