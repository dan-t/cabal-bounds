{-# LANGUAGE PatternGuards #-}

module CabalBounds.Drop
   ( drop
   ) where

import Prelude hiding (drop)
import qualified Distribution.PackageDescription as C
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import Control.Lens
import CabalBounds.Bound (Bound(..))
import CabalBounds.Targets (Targets(..), dependenciesOf)
import CabalBounds.Dependencies (Dependencies, filterDependencies)
import CabalBounds.Lenses
import Data.List (foldl')


drop :: Bound -> Targets -> Dependencies -> C.GenericPackageDescription -> C.GenericPackageDescription
drop bound AllTargets deps pkgDescrp =
   pkgDescrp & dependenciesOfLib        . filterDeps %~ dropFromDep
             & dependenciesOfAllExes    . filterDeps %~ dropFromDep
             & dependenciesOfAllTests   . filterDeps %~ dropFromDep
             & dependenciesOfAllBenchms . filterDeps %~ dropFromDep
   where
      filterDeps  = filterDependencies deps
      dropFromDep = dropFromDependency bound


drop bound (Targets targets) deps pkgDescrp =
   foldl' dropFromTarget pkgDescrp targets
   where
      dropFromTarget pkgDescrp target =
         pkgDescrp & (dependenciesOf target). filterDeps %~ dropFromDep

      filterDeps  = filterDependencies deps
      dropFromDep = dropFromDependency bound


dropFromDependency :: Bound -> C.Dependency -> C.Dependency
dropFromDependency UpperBound (C.Dependency pkgName versionRange) = C.Dependency pkgName versionRange'
   where
      versionRange'
         | Just vi <- C.mkVersionIntervals intervals' 
         = C.fromVersionIntervals vi

         | otherwise
         = versionRange

      intervals' = map (& _2 .~ C.NoUpperBound) (C.asVersionIntervals versionRange)

dropFromDependency _ (C.Dependency pkgName _) = C.Dependency pkgName C.anyVersion
