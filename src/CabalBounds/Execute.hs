{-# LANGUAGE PatternGuards #-}

module CabalBounds.Execute
   ( execute
   ) where

import Distribution.PackageDescription
import Distribution.Package
import Distribution.Version
import Control.Lens
import qualified CabalBounds.Command as Cmd
import CabalBounds.Lenses


execute :: Cmd.Command -> GenericPackageDescription -> GenericPackageDescription
execute (Cmd.Drop bound Cmd.AllTargets) pkgDescrp =
   pkgDescrp & libDependencies    %~ map (dropBound bound)
             & exeDependencies    %~ map (dropBound bound)
             & testDependencies   %~ map (dropBound bound)
             & benchmDependencies %~ map (dropBound bound)

execute cmd pkgDescrp = undefined


dropBound :: Cmd.Bound -> Dependency -> Dependency
dropBound Cmd.UpperBound (Dependency pkgName versionRange) = Dependency pkgName versionRange'
   where
      versionRange'
         | Just vi <- mkVersionIntervals intervals' 
         = fromVersionIntervals vi

         | otherwise
         = versionRange

      intervals' = map (\(lower, _) -> (lower, NoUpperBound)) (asVersionIntervals versionRange)

dropBound _ (Dependency pkgName _) = Dependency pkgName anyVersion
