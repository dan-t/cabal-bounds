{-# LANGUAGE PatternGuards #-}

module CabalBounds.Sections
   ( sections
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import CabalBounds.Args (Args)
import qualified CabalBounds.Args as A
import qualified CabalLenses as CL

sections :: Args -> GenericPackageDescription -> [CL.Section]
sections args pkgDescrp
   | ss@(_:_) <- concat [ [ CL.Library | A.library args ]
                        , map CL.Executable (A.executable args)
                        , map CL.TestSuite (A.testSuite args)
                        , map CL.Benchmark (A.benchmark args)
                        ]
   = ss

   | otherwise
   = CL.allSections pkgDescrp
