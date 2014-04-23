{-# LANGUAGE PatternGuards, Rank2Types #-}

module CabalBounds.Sections
   ( Sections(..)
   , Section(..)
   , sections
   , dependenciesOf
   ) where

import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Package (Dependency(..))
import CabalBounds.Args (Args)
import qualified CabalBounds.Args as A
import qualified CabalBounds.Lenses as L

data Sections = Sections [Section]
              | AllSections
              deriving (Show, Eq)

-- | A section of the cabal file.
data Section = Library
             | Executable String
             | TestSuite String
             | Benchmark String
             deriving (Show, Eq)

sections :: Args -> Sections
sections args
   | ts@(_:_) <- concat [ [ Library | A.library args ]
                        , map Executable (A.executable args)
                        , map TestSuite (A.testSuite args)
                        , map Benchmark (A.benchmark args)
                        ]
   = Sections ts

   | otherwise
   = AllSections


dependenciesOf :: Section -> Traversal' GenericPackageDescription [Dependency]
dependenciesOf Library            = L.dependenciesOfLib
dependenciesOf (Executable exe)   = L.dependenciesOfExe exe
dependenciesOf (TestSuite test)   = L.dependenciesOfTest test
dependenciesOf (Benchmark benchm) = L.dependenciesOfBenchm benchm
