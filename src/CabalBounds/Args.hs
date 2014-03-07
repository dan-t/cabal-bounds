{-# LANGUAGE DeriveDataTypeable, CPP #-}
{-# OPTIONS_GHC -w #-}

module CabalBounds.Args
   ( Args(..)
   , get
   , outputFile
   , defaultDrop
   , defaultUpdate
   ) where

import System.Console.CmdArgs hiding (ignore)
import CabalBounds.VersionComp (VersionComp(..))

#ifdef CABAL
import Data.Version (showVersion)
import Paths_cabal_bounds (version)
#endif

data Args = Drop { upper      :: Bool
                 , library    :: Bool
                 , executable :: [String]
                 , testSuite  :: [String]
                 , benchmark  :: [String]
                 , only       :: [String]
                 , ignore     :: [String]
                 , output     :: String
                 , cabalFile  :: String
                 }
          | Update { lower           :: Bool
                   , upper           :: Bool
                   , lowerComp       :: Maybe VersionComp
                   , upperComp       :: Maybe VersionComp
                   , library         :: Bool
                   , executable      :: [String]
                   , testSuite       :: [String]
                   , benchmark       :: [String]
                   , only            :: [String]
                   , ignore          :: [String]
                   , output          :: String
                   , cabalFile       :: String
                   , setupConfigFile :: String
                   } 
          deriving (Data, Typeable, Show, Eq)


get :: IO Args
get = cmdArgsRun . cmdArgsMode $ modes [dropArgs, updateArgs]
   &= program "cabal-bounds"
   &= summary summaryInfo
   &= help "A command line program for managing the bounds/versions of the dependencies in a cabal file."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo]
   where
      summaryInfo = ""


outputFile :: Args -> FilePath
outputFile args
   | null $ output args = cabalFile args
   | otherwise          = output args


defaultDrop :: Args
defaultDrop = Drop
   { upper           = def
   , library         = def
   , executable      = def
   , testSuite       = def
   , benchmark       = def
   , only            = def
   , ignore          = def
   , output          = def
   , cabalFile       = def
   }


defaultUpdate :: Args
defaultUpdate = Update
   { lower           = def
   , upper           = def
   , lowerComp       = def
   , upperComp       = def
   , library         = def
   , executable      = def
   , testSuite       = def
   , benchmark       = def
   , only            = def
   , ignore          = def
   , output          = def
   , cabalFile       = def
   , setupConfigFile = def
   }


dropArgs :: Args
dropArgs = Drop
   { upper           = def &= explicit &= name "upper" &= name "U" &= help "Only the upper bound is dropped, otherwise both - the lower and upper - bounds are dropped."
   , library         = def &= explicit &= name "library" &= name "l" &= help "Only the bounds of the library are modified."
   , executable      = def &= typ "EXECUTABLE" &= help "Only the bounds of the executable are modified."
   , testSuite       = def &= typ "TESTSUITE" &= help "Only the bounds of the test suite are modified."
   , benchmark       = def &= typ "BENCHMARK" &= help "Only the bounds of the benchmark are modified."
   , only            = def &= explicit &= typ "DEPENDENCY" &= name "only" &= name "O" &= help "Only the bounds of the dependency are modified."
   , ignore          = def &= explicit &= typ "DEPENDENCY" &= name "ignore" &= name "I" &= help "This dependency is ignored, not modified in any way."
   , output = def &= explicit &= typ "FILE" &= name "output" &= name "o" &= help "Save modified cabal file to file, if empty, the cabal file is modified inplace."
   , cabalFile       = def &= argPos 0 &= typ "CABAL-FILE"
   }


updateArgs :: Args
updateArgs = Update
   { lower           = def &= explicit &= name "lower" &= name "L" &= help "Only the lower bound is updated. The same as using '--lowercomp=minor'."
   , upper           = def &= explicit &= name "upper" &= name "U" &= help "Only the upper bound is updated. The same as using '--uppercomp=major2'."
   , lowerComp       = def &= explicit &= name "lowercomp" &= help "Only the lower bound is updated with the specified version component. (major1, major2 or minor)" 
   , upperComp       = def &= explicit &= name "uppercomp" &= help "Only the upper bound is updated with the specified version component. (major1, major2 or minor)"
   , setupConfigFile = def &= argPos 1 &= typ "SETUP-CONFIG-FILE"
   }


versionInfo :: String
versionInfo =
#ifdef CABAL
   "cabal-bounds version " ++ showVersion version
#else
   "cabal-bounds version unknown (not built with cabal)"
#endif


