{-# LANGUAGE PatternGuards #-}

module CabalBounds.Update
   ( update
   ) where

import qualified Distribution.PackageDescription as D
import qualified Distribution.Package as P
import qualified Distribution.Version as V
import qualified Distribution.Simple.LocalBuildInfo as BI
import qualified Distribution.Simple.PackageIndex as PX
import qualified Distribution.InstalledPackageInfo as PI
import Control.Lens
import Control.Applicative ((<$>))
import CabalBounds.Bound (UpdateBound(..))
import CabalBounds.Sections (Sections(..), dependenciesOf)
import CabalBounds.Dependencies (Dependencies, filterDependencies)
import CabalBounds.VersionComp (VersionComp(..), defaultLowerComp)
import qualified CabalBounds.Lenses as L
import Data.List (sort, foldl')
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, listToMaybe)

type PkgName           = String
type InstalledPackages = HM.HashMap PkgName V.Version


update :: UpdateBound -> Sections -> Dependencies -> D.GenericPackageDescription -> BI.LocalBuildInfo -> D.GenericPackageDescription
update bound AllSections deps pkgDescrp buildInfo =
   pkgDescrp & L.allDependencies . filterDeps %~ updateDep
   where
      filterDeps = filterDependencies deps
      updateDep  = updateDependency bound (installedPackages buildInfo)

update bound (Sections sections) deps pkgDescrp buildInfo =
   foldl' updateSection pkgDescrp sections
   where
      updateSection pkgDescrp section =
         pkgDescrp & (dependenciesOf section) . filterDeps %~ updateDep

      filterDeps = filterDependencies deps
      updateDep  = updateDependency bound (installedPackages buildInfo)


updateDependency :: UpdateBound -> InstalledPackages -> P.Dependency -> P.Dependency
updateDependency (UpdateLower comp) instPkgs dep =
   fromMaybe dep $ do
      let pkgName_ = pkgName dep
      version <- HM.lookup pkgName_ instPkgs
      vrange  <- mkVersionRange (comp `compOf` version) Nothing
      return $ mkDependency pkgName_ vrange

updateDependency (UpdateUpper comp) instPkgs dep
   | V.isAnyVersion versionRange_
   = updateDependency (UpdateBoth defaultLowerComp comp) instPkgs dep

   | otherwise
   = fromMaybe dep $ do
        upperVersion                <- HM.lookup pkgName_ instPkgs
        V.LowerBound lowerVersion _ <- fst <$> (listToMaybe $ V.asVersionIntervals versionRange_)
        vrange                      <- mkVersionRange lowerVersion (Just $ nextVersion $ comp `compOf` upperVersion)
        return $ mkDependency pkgName_ vrange
   where
      versionRange_ = versionRange dep
      pkgName_      = pkgName dep

updateDependency (UpdateBoth lowerComp upperComp) instPkgs dep =
   fromMaybe dep $ do
      let pkgName_ = pkgName dep
      version <- HM.lookup pkgName_ instPkgs
      vrange  <- mkVersionRange (lowerComp `compOf` version) (Just $ nextVersion $ upperComp `compOf` version)
      return $ mkDependency pkgName_ vrange


mkVersionRange :: V.Version -> Maybe V.Version -> Maybe V.VersionRange
mkVersionRange lowerVersion Nothing =
   V.fromVersionIntervals <$> V.mkVersionIntervals [(V.LowerBound lowerVersion V.InclusiveBound, V.NoUpperBound)]

mkVersionRange lowerVersion (Just upperVersion) =
   V.fromVersionIntervals <$> V.mkVersionIntervals [(V.LowerBound lowerVersion V.InclusiveBound,
                                                     V.UpperBound upperVersion V.ExclusiveBound)]


compOf :: VersionComp -> V.Version -> V.Version
Major1 `compOf` version =
   version & L.vbranch %~ take 1
           & L.vtags   .~ []

Major2 `compOf` version =
   version & L.vbranch %~ take 2
           & L.vtags   .~ []

Minor `compOf` version =
   version & L.vtags .~ []


nextVersion :: V.Version -> V.Version
nextVersion version =
   version & L.vbranch %~ increaseLastComp
   where
      increaseLastComp = reverse . (& ix 0 %~ (+ 1)) . reverse


installedPackages :: BI.LocalBuildInfo -> InstalledPackages
installedPackages = HM.fromList
                    . map (\(P.PackageName n, v) -> (n, newestVersion v))
                    . filter ((not . null) . snd)
                    . PX.allPackagesByName . BI.installedPkgs
   where
      newestVersion :: [PI.InstalledPackageInfo] -> V.Version
      newestVersion = last . sort . map (P.pkgVersion . PI.sourcePackageId)


pkgName :: P.Dependency -> PkgName
pkgName (P.Dependency (P.PackageName name) _) = name


versionRange :: P.Dependency -> V.VersionRange
versionRange (P.Dependency _ vrange) = vrange


mkDependency :: PkgName -> V.VersionRange -> P.Dependency
mkDependency name vrange = P.Dependency (P.PackageName name) vrange
