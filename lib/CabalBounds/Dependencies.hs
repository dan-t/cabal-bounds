{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Dependencies
   ( Dependencies(..)
   , dependencies
   , filterDependency
   ) where

import Control.Lens
import qualified CabalBounds.Args as A
import Distribution.Package (Dependency(..), PackageName(..))

-- | Which dependencies in the cabal file should the considered.
data Dependencies = AllDependencies              -- ^ all dependencies
                  | WithMissingBounds            -- ^ all dependencies having missing bounds
                  | OnlyDependencies [String]    -- ^ only the listed dependencies
                  | IgnoreDependencies [String]  -- ^ all dependencies but the listed ones
                  deriving (Show, Eq)


dependencies :: A.Args -> Dependencies
dependencies args
   | ds@(_:_) <- A.only args
   = OnlyDependencies ds

   | ds@(_:_) <- A.ignore args
   = IgnoreDependencies ds

   | A.Update {A.missing = missing} <- args
   , missing
   = WithMissingBounds

   | otherwise
   = AllDependencies


filterDependency :: Dependencies -> Traversal' Dependency Dependency
filterDependency AllDependencies =
   filtered (const True)

filterDependency WithMissingBounds =
   filtered (const True)

filterDependency (OnlyDependencies deps) =
   filtered (\(Dependency (PackageName pkgName) _) -> pkgName `elem` deps)

filterDependency (IgnoreDependencies deps) =
   filtered (\(Dependency (PackageName pkgName) _) -> pkgName `notElem` deps)
