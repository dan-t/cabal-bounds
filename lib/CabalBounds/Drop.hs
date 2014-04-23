{-# LANGUAGE PatternGuards #-}

module CabalBounds.Drop
   ( drop
   ) where

import Prelude hiding (drop)
import Control.Lens
import CabalBounds.Bound (DropBound(..))
import CabalBounds.Sections (Sections(..), dependenciesOf)
import CabalBounds.Dependencies (Dependencies, filterDependencies)
import qualified CabalBounds.Lenses as L
import Data.List (foldl')
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Package (Dependency(..))
import Distribution.Version (mkVersionIntervals, fromVersionIntervals, asVersionIntervals, UpperBound(..), anyVersion)


drop :: DropBound -> Sections -> Dependencies -> GenericPackageDescription -> GenericPackageDescription
drop bound AllSections deps pkgDescrp =
   pkgDescrp & L.allDependencies . filterDeps %~ dropFromDep
   where
      filterDeps  = filterDependencies deps
      dropFromDep = dropFromDependency bound


drop bound (Sections sections) deps pkgDescrp =
   foldl' dropFromSection pkgDescrp sections
   where
      dropFromSection pkgDescrp section =
         pkgDescrp & dependenciesOf section . filterDeps %~ dropFromDep

      filterDeps  = filterDependencies deps
      dropFromDep = dropFromDependency bound


dropFromDependency :: DropBound -> Dependency -> Dependency
dropFromDependency DropUpper (Dependency pkgName versionRange) = Dependency pkgName versionRange'
   where
      versionRange'
         | Just vi <- mkVersionIntervals intervals'
         = fromVersionIntervals vi

         | otherwise
         = versionRange

      intervals' = map (& _2 .~ NoUpperBound) (asVersionIntervals versionRange)

dropFromDependency DropBoth (Dependency pkgName _) = Dependency pkgName anyVersion
