{-# LANGUAGE TemplateHaskell #-}

module CabalBounds.Lenses where

import Distribution.PackageDescription
import Distribution.Package
import Distribution.Version
import Control.Lens

makeLensesFor [ ("condLibrary"    , "condLibraryL")
              , ("condExecutables", "condExecutablesL")
              , ("condTestSuites" , "condTestSuitesL")
              , ("condBenchmarks" , "condBenchmarksL")
              ] ''GenericPackageDescription

makeLensesFor [ ("condTreeConstraints", "condTreeConstraintsL")
              ] ''CondTree

makeLensesFor [ ("exeName", "exeNameL")
              ] ''Executable

makeLensesFor [ ("testName", "testNameL")
              ] ''TestSuite

makeLensesFor [ ("benchmarkName", "benchmarkNameL")
              ] ''Benchmark
 
libDependencies :: Traversal' GenericPackageDescription [Dependency]
libDependencies = condLibraryL . _Just . condTreeConstraintsL

exeDependencies :: Traversal' GenericPackageDescription [Dependency]
exeDependencies = condExecutablesL . traversed . _2 . condTreeConstraintsL

testDependencies :: Traversal' GenericPackageDescription [Dependency]
testDependencies = condTestSuitesL . traversed . _2 . condTreeConstraintsL

benchmDependencies :: Traversal' GenericPackageDescription [Dependency]
benchmDependencies = condBenchmarksL . traversed . _2 . condTreeConstraintsL
