{-# LANGUAGE DeriveDataTypeable, CPP #-}

module CabalBounds.Args
   ( Args(..)
   , get
   , outputFile
   ) where

import System.Console.CmdArgs

#ifdef CABAL
import Data.Version (showVersion)
import Paths_cabal_bounds (version)
#endif

data Args = Drop { upper           :: Bool
                 , library         :: Bool
                 , executable      :: [String]
                 , testSuite       :: [String]
                 , benchmark       :: [String]
                 , outputCabalFile :: String
                 , cabalFile       :: String
                 }
          | Update { lower           :: Bool
                   , upper           :: Bool
                   , library         :: Bool
                   , executable      :: [String]
                   , testSuite       :: [String]
                   , benchmark       :: [String]
                   , outputCabalFile :: String
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
   | null $ outputCabalFile args = cabalFile args
   | otherwise                   = outputCabalFile args


dropArgs :: Args
dropArgs = Drop
   { upper           = def &= explicit &= name "upper" &= name "U" &= help "Only the upper bound is dropped, otherwise both - the lower and upper - bounds are dropped."
   , library         = def &= explicit &= name "library" &= name "l" &= help "Only the bounds of the library are dropped."
   , executable      = def &= help "Only the bounds of the executable are dropped."
   , testSuite       = def &= help "Only the bounds of the test suite are dropped."
   , benchmark       = def &= help "Only the bounds of the benchmark are dropped."
   , outputCabalFile = def &= help "Save modified cabal file to file, if empty, the cabal file is modified inplace."
   , cabalFile       = def &= argPos 0 &= typ "CABAL-FILE"
   }


updateArgs :: Args
updateArgs = Update
   { lower           = def &= explicit &= name "lower" &= name "L" &= help "Only the lower bound is updated."
   , upper           = def &= explicit &= name "upper" &= name "U" &= help "Only the upper bound is updated."
   , library         = def &= explicit &= name "library" &= name "l" &= help "Only the bounds of the library are updated."
   , executable      = def &= help "Only the bounds of the executable are updated."
   , testSuite       = def &= help "Only the bounds of the test suite are updated."
   , benchmark       = def &= help "Only the bounds of the benchmark are updated."
   , outputCabalFile = def &= help "Save modified cabal file to file, if empty, the cabal file is modified inplace."
   , cabalFile       = def &= argPos 0 &= typ "CABAL-FILE"
   , setupConfigFile = def &= argPos 1 &= typ "SETUP-CONFIG-FILE"
   }


versionInfo :: String
versionInfo =
#ifdef CABAL
   "cabal-bounds version " ++ showVersion version
#else
   "cabal-bounds version unknown (not built with cabal)"
#endif


