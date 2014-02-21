{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Targets
   ( Targets(..)
   , Target(..)
   , targets
   , dependenciesOf
   ) where

import Control.Lens
import qualified Distribution.PackageDescription as C
import qualified Distribution.Package as C
import CabalBounds.Args
import CabalBounds.Lenses

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
   | ts@(_:_) <- concat [ if (library args) then [Library] else []
                        , map Executable (executable args)
                        , map TestSuite (testSuite args)
                        , map Benchmark (benchmark args)
                        ]
   = Targets ts

   | otherwise
   = AllTargets


dependenciesOf :: Target -> Traversal' C.GenericPackageDescription [C.Dependency]
dependenciesOf Library            = dependenciesOfLib
dependenciesOf (Executable exe)   = dependenciesOfExe exe
dependenciesOf (TestSuite test)   = dependenciesOfTest test
dependenciesOf (Benchmark benchm) = dependenciesOfBenchm benchm
