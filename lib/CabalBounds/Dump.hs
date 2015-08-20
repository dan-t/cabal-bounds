
module CabalBounds.Dump
   ( dump
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified CabalLenses as CL
import CabalBounds.Dependencies (Dependencies(..), allDependency, filterDependency)
import Control.Lens

type LibName    = String
type LibVersion = [Int]
type Library    = (LibName, LibVersion)


dump :: Dependencies -> [GenericPackageDescription] -> [Library]
dump deps pkgDescrps = HM.toList $ foldl' addLibsFromPkgDescrp HM.empty pkgDescrps
   where
      addLibsFromPkgDescrp libs pkgDescrp =
         foldl' addLibFromDep libs (pkgDescrp ^.. allDependency . filterDep)
         where
            filterDep = filterDependency deps

      addLibFromDep libs dep
         | lowerBound_ /= CL.noLowerBound
         = HM.insertWith min pkgName_ versionBranch_ libs

         | otherwise
         = libs
         where
            pkgName_       = dep ^. CL.packageName . _Wrapped
            versionBranch_ = lowerBound_ ^. CL.version . CL.versionBranchL
            lowerBound_    = fromMaybe CL.noLowerBound (dep ^? CL.versionRange . CL.intervals . _head . CL.lowerBound)
