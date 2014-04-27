{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Dependencies
   ( Dependencies(..)
   , dependencies
   , filterDependencies
   ) where

import Control.Lens
import qualified CabalBounds.Args as A
import Distribution.Package (Dependency(..), PackageName(..))

data Dependencies = AllDependencies
                  | OnlyDependencies [String]
                  | IgnoreDependencies [String]
                  deriving (Show, Eq)


dependencies :: A.Args -> Dependencies
dependencies args
   | ds@(_:_) <- A.only args
   = OnlyDependencies ds

   | ds@(_:_) <- A.ignore args
   = IgnoreDependencies ds

   | otherwise
   = AllDependencies


filterDependencies :: Dependencies -> Traversal' [Dependency] Dependency
filterDependencies AllDependencies =
   traversed

filterDependencies (OnlyDependencies deps) =
   traversed . filtered (\(Dependency (PackageName pkgName) _) -> pkgName `elem` deps)

filterDependencies (IgnoreDependencies deps) =
   traversed . filtered (\(Dependency (PackageName pkgName) _) -> pkgName `notElem` deps)
