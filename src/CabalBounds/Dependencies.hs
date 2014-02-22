{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Dependencies
   ( Dependencies(..)
   , dependencies
   , filterDependencies
   ) where

import Control.Lens 
import qualified CabalBounds.Args as A
import qualified Distribution.Package as C

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


filterDependencies :: Dependencies -> Traversal' [C.Dependency] C.Dependency
filterDependencies AllDependencies =
   traversed . filtered (const True)

filterDependencies (OnlyDependencies deps) =
   traversed . filtered (\(C.Dependency (C.PackageName pkgName) _) -> any (== pkgName) deps) 

filterDependencies (IgnoreDependencies deps) =
   traversed . filtered (\(C.Dependency (C.PackageName pkgName) _) -> all (/= pkgName) deps)
