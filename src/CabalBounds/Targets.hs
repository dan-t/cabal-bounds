{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Targets
   ( Targets(..)
   , Target(..)
   , targets
   , dependenciesOf
   ) where

import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Package (Dependency(..))
import CabalBounds.Args (Args)
import qualified CabalBounds.Args as A
import qualified CabalBounds.Lenses as L

data Targets = Targets [Target]
             | AllTargets
             deriving (Show, Eq)

data Target = Library
            | Executable String
            | TestSuite String
            | Benchmark String
            deriving (Show, Eq)

targets :: Args -> Targets
targets args 
   | ts@(_:_) <- concat [ if (A.library args) then [Library] else []
                        , map Executable (A.executable args)
                        , map TestSuite (A.testSuite args)
                        , map Benchmark (A.benchmark args)
                        ]
   = Targets ts

   | otherwise
   = AllTargets


dependenciesOf :: Target -> Traversal' GenericPackageDescription [Dependency]
dependenciesOf Library            = L.dependenciesOfLib
dependenciesOf (Executable exe)   = L.dependenciesOfExe exe
dependenciesOf (TestSuite test)   = L.dependenciesOfTest test
dependenciesOf (Benchmark benchm) = L.dependenciesOfBenchm benchm
