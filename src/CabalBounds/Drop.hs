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
import CabalBounds.Targets (Targets(..), Target(..), dependenciesOf)
import CabalBounds.Lenses
import Data.List (foldl')


drop :: Bound -> Targets -> C.GenericPackageDescription -> C.GenericPackageDescription
drop bound AllTargets pkgDescrp =
   pkgDescrp & dependenciesOfLib        %~ map (dropBound bound)
             & dependenciesOfAllExes    %~ map (dropBound bound)
             & dependenciesOfAllTests   %~ map (dropBound bound)
             & dependenciesOfAllBenchms %~ map (dropBound bound)

drop bound (Targets targets) pkgDescrp =
   foldl' dropFromTarget pkgDescrp targets
   where
      dropFromTarget pkgDescrp target =
         pkgDescrp & (dependenciesOf target) %~ map (dropBound bound)


dropBound :: Bound -> C.Dependency -> C.Dependency
dropBound UpperBound (C.Dependency pkgName versionRange) = C.Dependency pkgName versionRange'
   where
      versionRange'
         | Just vi <- C.mkVersionIntervals intervals' 
         = C.fromVersionIntervals vi

         | otherwise
         = versionRange

      intervals' = map (& _2 .~ C.NoUpperBound) (C.asVersionIntervals versionRange)

dropBound _ (C.Dependency pkgName _) = C.Dependency pkgName C.anyVersion
