{-# LANGUAGE PatternGuards, Rank2Types #-}

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
   pkgDescrp & dependenciesOfLib        %~ map (dropBound bound)
             & dependenciesOfAllExes    %~ map (dropBound bound)
             & dependenciesOfAllTests   %~ map (dropBound bound)
             & dependenciesOfAllBenchms %~ map (dropBound bound)

execute (Cmd.Drop bound (Cmd.Targets targets)) pkgDescrp =
   foldl' dropFromTarget pkgDescrp targets
   where
      dropFromTarget pkgDescrp target =
         pkgDescrp & (dependenciesOf target) %~ map (dropBound bound)


dropBound :: Cmd.Bound -> Dependency -> Dependency
dropBound Cmd.UpperBound (Dependency pkgName versionRange) = Dependency pkgName versionRange'
   where
      versionRange'
         | Just vi <- mkVersionIntervals intervals' 
         = fromVersionIntervals vi

         | otherwise
         = versionRange

      intervals' = map (& _2 .~ NoUpperBound) (asVersionIntervals versionRange)

dropBound _ (Dependency pkgName _) = Dependency pkgName anyVersion


dependenciesOf :: Cmd.Target -> Traversal' GenericPackageDescription [Dependency]
dependenciesOf Cmd.Library            = dependenciesOfLib
dependenciesOf (Cmd.Executable exe)   = dependenciesOfExe exe
dependenciesOf (Cmd.TestSuite test)   = dependenciesOfTest test
dependenciesOf (Cmd.Benchmark benchm) = dependenciesOfBenchm benchm
