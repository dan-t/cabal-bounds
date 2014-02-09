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
import Data.List (foldl')


execute :: Cmd.Command -> GenericPackageDescription -> GenericPackageDescription
execute (Cmd.Drop bound Cmd.AllTargets) pkgDescrp =
   pkgDescrp & libDependencies          %~ map (dropBound bound)
             & dependenciesOfAllExes    %~ map (dropBound bound)
             & dependenciesOfAllTests   %~ map (dropBound bound)
             & dependenciesOfAllBenchms %~ map (dropBound bound)

execute (Cmd.Drop bound (Cmd.Targets targets)) pkgDescrp =
   foldl' dropFromTarget pkgDescrp targets
   where
      dropFromTarget pkgDescrp Cmd.Library =
         pkgDescrp & libDependencies %~ map (dropBound bound)

      dropFromTarget pkgDescrp (Cmd.Executable exe) =
         pkgDescrp & dependenciesOfExe exe %~ map (dropBound bound)

      dropFromTarget pkgDescrp (Cmd.TestSuite test) =
         pkgDescrp & dependenciesOfTest test %~ map (dropBound bound)

      dropFromTarget pkgDescrp (Cmd.Benchmark benchm) =
         pkgDescrp & dependenciesOfBenchm benchm %~ map (dropBound bound)


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
