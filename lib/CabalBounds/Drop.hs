{-# LANGUAGE PatternGuards #-}

module CabalBounds.Drop
   ( drop
   ) where

import Prelude hiding (drop)
import Control.Lens
import CabalBounds.Bound (DropBound(..))
import CabalBounds.Dependencies (Dependencies, filterDependency)
import qualified CabalLenses as CL
import Data.List (foldl')
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Package (Dependency(..))
import Distribution.Version (mkVersionIntervals, fromVersionIntervals, asVersionIntervals, UpperBound(..), anyVersion)


drop :: DropBound -> [CL.Section] -> Dependencies -> GenericPackageDescription -> GenericPackageDescription
drop bound sections deps pkgDescrp =
   foldl' dropFromSection pkgDescrp sections
   where
      dropFromSection pkgDescrp section =
         pkgDescrp & CL.dependencyIf condVars section . filterDep %~ dropFromDep

      filterDep   = filterDependency deps
      dropFromDep = dropFromDependency bound
      condVars    = CL.fromDefaults pkgDescrp


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
