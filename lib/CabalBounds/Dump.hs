
module CabalBounds.Dump
   ( dump
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.Version as V
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified CabalLenses as CL
import Control.Lens

type LibName    = String
type LibVersion = [Int]
type Library    = (LibName, LibVersion)


dump :: [GenericPackageDescription] -> [Library]
dump pkgDescrps = HM.toList $ foldl' addLibsFromPkgDescrp HM.empty pkgDescrps
   where
      addLibsFromPkgDescrp libs pkgDescrp = foldl' addLibFromDep libs (pkgDescrp ^.. CL.allDependency)

      addLibFromDep libs dep
         | depLowerBound /= CL.noLowerBound
         = HM.insertWith min depPkgName lowerVersionBranch libs

         | otherwise
         = libs
         where
            depPkgName = dep ^. CL.depPackageName . CL.pkgNameString

            depLowerBound@(V.LowerBound (V.Version { V.versionBranch = lowerVersionBranch }) _) =
               fromMaybe CL.noLowerBound (dep ^? CL.depVersionRange . CL.rangeToIntervals . _head . CL.lowerBound)
