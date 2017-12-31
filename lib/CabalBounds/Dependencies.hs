{-# LANGUAGE PatternGuards, Rank2Types, CPP #-}

module CabalBounds.Dependencies
   ( Dependencies(..)
   , dependencies
   , filterDependency
   , allDependency
   , dependencyIf
   , filterLibrary
   ) where

import Control.Lens
import qualified CabalBounds.Args as A
import qualified CabalLenses as CL
import CabalBounds.Types
import Distribution.Package (Dependency(..), unPackageName)
import Distribution.PackageDescription (GenericPackageDescription)

-- | Which dependencies in the cabal file should the considered.
data Dependencies = AllDependencies              -- ^ all dependencies
                  | OnlyDependencies [String]    -- ^ only the listed dependencies
                  | IgnoreDependencies [String]  -- ^ all dependencies but the listed ones
                  deriving (Show, Eq)


dependencies :: A.Args -> Dependencies
dependencies args
   | ds@(_:_) <- A.only args
   = OnlyDependencies ds

   | ds@(_:_) <- A.ignore args
   = IgnoreDependencies ds

   | otherwise
   = AllDependencies


filterDependency :: Dependencies -> Traversal' Dependency Dependency
filterDependency AllDependencies =
   filtered (const True)

filterDependency (OnlyDependencies deps) =
   filtered (\(Dependency pkg _) -> (unPackageName pkg) `elem` deps)

filterDependency (IgnoreDependencies deps) =
   filtered (\(Dependency pkg _) -> (unPackageName pkg) `notElem` deps)


-- | A traversal for all 'Dependency' of all 'Section'.
allDependency :: Traversal' GenericPackageDescription Dependency
allDependency =
#if MIN_VERSION_Cabal(1,22,1)
   CL.allBuildInfo . CL.targetBuildDependsL . traversed
#else
   CL.allDependency
#endif


-- | A traversal for the 'Dependency' of 'Section' that match 'CondVars'.
dependencyIf :: CL.CondVars -> CL.Section -> Traversal' GenericPackageDescription Dependency
dependencyIf condVars section =
#if MIN_VERSION_Cabal(1,22,1)
   CL.buildInfoIf condVars section . CL.targetBuildDependsL . traversed
#else
   CL.dependencyIf condVars section
#endif


filterLibrary :: Dependencies -> Traversal' Library Library
filterLibrary AllDependencies =
   filtered (const True)

filterLibrary (OnlyDependencies deps) =
   filtered (\(libName, _) -> libName `elem` deps)

filterLibrary (IgnoreDependencies deps) =
   filtered (\(libName, _) -> libName `notElem` deps)
