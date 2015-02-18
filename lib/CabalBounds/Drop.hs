{-# LANGUAGE PatternGuards #-}

module CabalBounds.Drop
   ( drop
   ) where

import Prelude hiding (drop)
import Control.Lens
import CabalBounds.Bound (DropBound(..))
import CabalBounds.Dependencies (Dependencies, filterDependency, dependencyIf)
import qualified CabalLenses as CL
import Data.List (foldl')
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Package (Dependency(..))
import Distribution.Version (UpperBound(..), anyVersion)


drop :: DropBound -> [CL.Section] -> Dependencies -> GenericPackageDescription -> GenericPackageDescription
drop bound sections deps pkgDescrp =
   foldl' dropFromSection pkgDescrp sections
   where
      dropFromSection pkgDescrp section =
         pkgDescrp & dependencyIf condVars section . filterDep %~ dropFromDep

      filterDep   = filterDependency deps
      dropFromDep = dropFromDependency bound
      condVars    = CL.fromDefaults pkgDescrp


dropFromDependency :: DropBound -> Dependency -> Dependency
dropFromDependency DropUpper dep = dep & CL.versionRange . CL.intervals . traversed . CL.upperBound .~ NoUpperBound
dropFromDependency DropBoth  dep = dep & CL.versionRange .~ anyVersion
