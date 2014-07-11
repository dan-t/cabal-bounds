
module CabalBounds.HaskellPlatform
   ( librariesOf
   , currentLibraries
   , previousLibraries
   , allVersions
   , HPVersion
   ) where

import qualified Distribution.Version as V
import Control.Applicative ((<$>))
import Data.List (find)

type LibName    = String
type LibVersion = V.Version
type Library    = (LibName, LibVersion)
type HPVersion  = String


-- | the libraries of the given haskell platform version
librariesOf :: HPVersion -> Maybe [Library]
librariesOf hpVers = snd <$> find ((== hpVers) . fst) allVersions


-- | the libraries of the current haskell platform
currentLibraries :: [Library]
currentLibraries = snd . last $ allVersions


-- | the libraries of the previous haskell platform
previousLibraries :: [Library]
previousLibraries = snd . head . drop 1 . reverse $ allVersions


-- | all haskell platform versions and their libraries
allVersions :: [(HPVersion, [Library])]
allVersions =
   [ ("2010.2.0.0", libs_2010_2_0_0)
   , ("2011.2.0.0", libs_2011_2_0_0)
   , ("2011.2.0.1", libs_2011_2_0_1)
   , ("2011.4.0.0", libs_2011_4_0_0)
   , ("2012.2.0.0", libs_2012_2_0_0)
   , ("2012.4.0.0", libs_2012_4_0_0)
   , ("2013.2.0.0", libs_2013_2_0_0)
   ]


libs_2010_2_0_0 =
   [ lib "array" [0,3,0,1]
   , lib "base" [4,2,0,2]
   , lib "bytestring" [0,9,1,7]
   , lib "Cabal" [1,8,0,6]
   , lib "cgi" [3001,1,7,3]
   , lib "containers" [0,3,0,0]
   , lib "deepseq" [1,1,0,0]
   , lib "directory" [1,0,1,1]
   , lib "extensible-exceptions" [0,1,1,1]
   , lib "fgl" [5,4,2,3]
   , lib "filepath" [1,1,0,4]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,3]
   , lib "haskell98" [1,0,1,1]
   , lib "hpc" [0,5,0,5]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,0,9]
   , lib "HUnit" [1,2,2,1]
   , lib "mtl" [1,1,0,2]
   , lib "network" [2,2,1,7]
   , lib "old-locale" [1,0,0,2]
   , lib "old-time" [1,0,0,5]
   , lib "OpenGL" [2,2,3,0]
   , lib "parallel" [2,2,0,1]
   , lib "parsec" [2,1,0,1]
   , lib "pretty" [1,0,1,1]
   , lib "process" [1,0,1,3]
   , lib "QuickCheck" [2,1,1,1]
   , lib "random" [1,0,0,2]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,93,1]
   , lib "regex-posix" [0,94,2]
   , lib "stm" [2,1,2,1]
   , lib "syb" [0,1,0,2]
   , lib "template-haskell" [2,4,0,1]
   , lib "time" [1,1,4]
   , lib "unix" [2,4,0,2]
   , lib "xhtml" [3000,2,0,1]
   , lib "zlib" [0,5,2,0]
   ]


libs_2011_2_0_0 =
   [ lib "array" [0,3,0,2]
   , lib "base" [4,3,1,0]
   , lib "bytestring" [0,9,1,10]
   , lib "Cabal" [1,10,1,0]
   , lib "cgi" [3001,1,7,4]
   , lib "containers" [0,4,0,0]
   , lib "deepseq" [1,1,0,2]
   , lib "directory" [1,1,0,0]
   , lib "extensible-exceptions" [0,1,1,2]
   , lib "fgl" [5,4,2,3]
   , lib "filepath" [1,2,0,0]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,4]
   , lib "haskell2010" [1,0,0,0]
   , lib "haskell98" [1,1,0,1]
   , lib "hpc" [0,5,0,6]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,1,1]
   , lib "HUnit" [1,2,2,3]
   , lib "mtl" [2,0,1,0]
   , lib "network" [2,3,0,2]
   , lib "old-locale" [1,0,0,2]
   , lib "old-time" [1,0,0,6]
   , lib "OpenGL" [2,2,3,0]
   , lib "parallel" [3,1,0,1]
   , lib "parsec" [3,1,1]
   , lib "pretty" [1,0,1,2]
   , lib "process" [1,0,1,5]
   , lib "QuickCheck" [2,4,0,1]
   , lib "random" [1,0,0,3]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,93,1]
   , lib "regex-posix" [0,94,4]
   , lib "stm" [2,2,0,1]
   , lib "syb" [0,3]
   , lib "template-haskell" [2,5,0,0]
   , lib "text" [0,11,0,5]
   , lib "time" [1,2,0,3]
   , lib "transformers" [0,2,2,0]
   , lib "unix" [2,4,2,0]
   , lib "xhtml" [3000,2,0,1]
   , lib "zlib" [0,5,3,1]
   ]


libs_2011_2_0_1 =
   [ lib "array" [0,3,0,2]
   , lib "base" [4,3,1,0]
   , lib "bytestring" [0,9,1,10]
   , lib "Cabal" [1,10,1,0]
   , lib "cgi" [3001,1,7,4]
   , lib "containers" [0,4,0,0]
   , lib "deepseq" [1,1,0,2]
   , lib "directory" [1,1,0,0]
   , lib "extensible-exceptions" [0,1,1,2]
   , lib "fgl" [5,4,2,3]
   , lib "filepath" [1,2,0,0]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,4]
   , lib "haskell2010" [1,0,0,0]
   , lib "haskell98" [1,1,0,1]
   , lib "hpc" [0,5,0,6]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,1,1]
   , lib "HUnit" [1,2,2,3]
   , lib "mtl" [2,0,1,0]
   , lib "network" [2,3,0,2]
   , lib "old-locale" [1,0,0,2]
   , lib "old-time" [1,0,0,6]
   , lib "OpenGL" [2,2,3,0]
   , lib "parallel" [3,1,0,1]
   , lib "parsec" [3,1,1]
   , lib "pretty" [1,0,1,2]
   , lib "process" [1,0,1,5]
   , lib "QuickCheck" [2,4,0,1]
   , lib "random" [1,0,0,3]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,93,1]
   , lib "regex-posix" [0,94,4]
   , lib "stm" [2,2,0,1]
   , lib "syb" [0,3]
   , lib "template-haskell" [2,5,0,0]
   , lib "text" [0,11,0,6]
   , lib "time" [1,2,0,3]
   , lib "transformers" [0,2,2,0]
   , lib "unix" [2,4,2,0]
   , lib "xhtml" [3000,2,0,1]
   , lib "zlib" [0,5,3,1]
   ]


libs_2011_4_0_0 =
   [ lib "array" [0,3,0,2]
   , lib "base" [4,3,1,0]
   , lib "bytestring" [0,9,1,10]
   , lib "Cabal" [1,10,2,0]
   , lib "cgi" [3001,1,7,4]
   , lib "containers" [0,4,0,0]
   , lib "deepseq" [1,1,0,2]
   , lib "directory" [1,1,0,0]
   , lib "extensible-exceptions" [0,1,1,2]
   , lib "fgl" [5,4,2,4]
   , lib "filepath" [1,2,0,0]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,4]
   , lib "haskell2010" [1,0,0,0]
   , lib "haskell98" [1,1,0,1]
   , lib "hpc" [0,5,0,6]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,1,2]
   , lib "HUnit" [1,2,4,2]
   , lib "mtl" [2,0,1,0]
   , lib "network" [2,3,0,5]
   , lib "old-locale" [1,0,0,2]
   , lib "old-time" [1,0,0,6]
   , lib "OpenGL" [2,2,3,0]
   , lib "parallel" [3,1,0,1]
   , lib "parsec" [3,1,1]
   , lib "pretty" [1,0,1,2]
   , lib "process" [1,0,1,5]
   , lib "QuickCheck" [2,4,1,1]
   , lib "random" [1,0,0,3]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,95,1]
   , lib "regex-posix" [0,95,1]
   , lib "stm" [2,2,0,1]
   , lib "syb" [0,3,3]
   , lib "template-haskell" [2,5,0,0]
   , lib "text" [0,11,1,5]
   , lib "time" [1,2,0,3]
   , lib "transformers" [0,2,2,0]
   , lib "unix" [2,4,2,0]
   , lib "xhtml" [3000,2,0,4]
   , lib "zlib" [0,5,3,1]
   ]


libs_2012_2_0_0 =
   [ lib "array" [0,4,0,0]
   , lib "base" [4,5,0,0]
   , lib "bytestring" [0,9,2,1]
   , lib "Cabal" [1,14,0]
   , lib "cgi" [3001,1,7,4]
   , lib "containers" [0,4,2,1]
   , lib "deepseq" [1,3,0,0]
   , lib "directory" [1,1,0,2]
   , lib "extensible-exceptions" [0,1,1,4]
   , lib "fgl" [5,4,2,4]
   , lib "filepath" [1,3,0,0]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,5]
   , lib "haskell2010" [1,1,0,1]
   , lib "haskell98" [2,0,0,1]
   , lib "hpc" [0,5,1,1]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,2,3]
   , lib "HUnit" [1,2,4,2]
   , lib "mtl" [2,1,1]
   , lib "network" [2,3,0,13]
   , lib "old-locale" [1,0,0,4]
   , lib "old-time" [1,1,0,0]
   , lib "OpenGL" [2,2,3,1]
   , lib "parallel" [3,2,0,2]
   , lib "parsec" [3,1,2]
   , lib "pretty" [1,1,1,0]
   , lib "process" [1,1,0,1]
   , lib "QuickCheck" [2,4,2]
   , lib "random" [1,0,1,1]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,95,1]
   , lib "regex-posix" [0,95,1]
   , lib "stm" [2,3]
   , lib "syb" [0,3,6,1]
   , lib "template-haskell" [2,7,0,0]
   , lib "text" [0,11,2,0]
   , lib "time" [1,4]
   , lib "transformers" [0,3,0,0]
   , lib "unix" [2,5,1,0]
   , lib "xhtml" [3000,2,1]
   , lib "zlib" [0,5,3,3]
   ]


libs_2012_4_0_0 =
   [ lib "array" [0,4,0,0]
   , lib "async" [2,0,1,3]
   , lib "base" [4,5,1,0]
   , lib "bytestring" [0,9,2,1]
   , lib "Cabal" [1,14,0]
   , lib "cgi" [3001,1,7,4]
   , lib "containers" [0,4,2,1]
   , lib "deepseq" [1,3,0,0]
   , lib "directory" [1,1,0,2]
   , lib "extensible-exceptions" [0,1,1,4]
   , lib "fgl" [5,4,2,4]
   , lib "filepath" [1,3,0,0]
   , lib "GLUT" [2,1,2,1]
   , lib "haskell-src" [1,0,1,5]
   , lib "haskell2010" [1,1,0,1]
   , lib "haskell98" [2,0,0,1]
   , lib "hpc" [0,5,1,1]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,2,5]
   , lib "HUnit" [1,2,5,1]
   , lib "mtl" [2,1,2]
   , lib "network" [2,3,1,0]
   , lib "old-locale" [1,0,0,4]
   , lib "old-time" [1,1,0,0]
   , lib "OpenGL" [2,2,3,1]
   , lib "parallel" [3,2,0,3]
   , lib "parsec" [3,1,3]
   , lib "pretty" [1,1,1,0]
   , lib "primitive" [0,5,0,1]
   , lib "process" [1,1,0,1]
   , lib "QuickCheck" [2,5,1,1]
   , lib "random" [1,0,1,1]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,95,1]
   , lib "regex-posix" [0,95,2]
   , lib "split" [0,2,1,1]
   , lib "stm" [2,4]
   , lib "syb" [0,3,7]
   , lib "template-haskell" [2,7,0,0]
   , lib "text" [0,11,2,3]
   , lib "time" [1,4]
   , lib "transformers" [0,3,0,0]
   , lib "unix" [2,5,1,1]
   , lib "vector" [0,10,0,1]
   , lib "xhtml" [3000,2,1]
   , lib "zlib" [0,5,4,0]
   ]


libs_2013_2_0_0 =
   [ lib "array" [0,4,0,1]
   , lib "async" [2,0,1,4]
   , lib "attoparsec" [0,10,4,0]
   , lib "base" [4,6,0,1]
   , lib "bytestring" [0,10,0,2]
   , lib "Cabal" [1,16,0]
   , lib "case-insensitive" [1,0,0,1]
   , lib "cgi" [3001,1,7,5]
   , lib "containers" [0,4,2,1]
   , lib "deepseq" [1,3,0,0]
   , lib "directory" [1,2,0,1]
   , lib "fgl" [5,4,2,4]
   , lib "filepath" [1,3,0,1]
   , lib "GLUT" [2,4,0,0]
   , lib "GLUTRaw" [1,3,0,0]
   , lib "hashable" [1,1,2,5]
   , lib "haskell-src" [1,0,1,5]
   , lib "haskell2010" [1,1,1,0]
   , lib "haskell98" [2,0,0,1]
   , lib "hpc" [0,6,0,0]
   , lib "html" [1,0,1,2]
   , lib "HTTP" [4000,2,8]
   , lib "HUnit" [1,2,5,2]
   , lib "mtl" [2,1,2]
   , lib "network" [2,4,1,2]
   , lib "old-locale" [1,0,0,5]
   , lib "old-time" [1,1,0,1]
   , lib "OpenGL" [2,8,0,0]
   , lib "OpenGLRaw" [1,3,0,0]
   , lib "parallel" [3,2,0,3]
   , lib "parsec" [3,1,3]
   , lib "pretty" [1,1,1,0]
   , lib "primitive" [0,5,0,1]
   , lib "process" [1,1,0,2]
   , lib "QuickCheck" [2,6]
   , lib "random" [1,0,1,1]
   , lib "regex-base" [0,93,2]
   , lib "regex-compat" [0,95,1]
   , lib "regex-posix" [0,95,2]
   , lib "split" [0,2,2]
   , lib "stm" [2,4,2]
   , lib "syb" [0,4,0]
   , lib "template-haskell" [2,8,0,0]
   , lib "text" [0,11,3,1]
   , lib "time" [1,4,0,1]
   , lib "transformers" [0,3,0,0]
   , lib "unordered-containers" [0,2,3,0]
   , lib "unix" [2,6,0,1]
   , lib "Win32" [2,3,0,0]
   , lib "vector" [0,10,0,1]
   , lib "xhtml" [3000,2,1]
   , lib "zlib" [0,5,4,1]
   ]


type VersionBranch = [Int]

lib :: LibName -> VersionBranch -> Library
lib libName branch = (libName, V.Version { V.versionBranch = branch , V.versionTags = [] })
