{-# LANGUAGE PatternGuards #-}

module CabalBounds.Update
   ( update
   ) where

import qualified Distribution.PackageDescription as C
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import qualified Distribution.Simple.LocalBuildInfo as C
import Control.Lens
import CabalBounds.Bound (Bound(..))
import CabalBounds.Targets (Targets(..), Target(..), dependenciesOf)
import CabalBounds.Lenses


update :: Bound -> Targets -> C.GenericPackageDescription -> C.LocalBuildInfo -> C.GenericPackageDescription
update _ _ pkgDescrp _ = pkgDescrp
