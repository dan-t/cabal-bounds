
module CabalBounds.Types where

import qualified Data.HashMap.Strict as HM
import qualified Distribution.Version as V
import Control.Lens
import qualified CabalLenses as CL

type LibName    = String
type LibVersion = [Int]
type Library    = (LibName, LibVersion)
type Libraries  = [Library]
type LibraryMap = HM.HashMap LibName V.Version

toList :: LibraryMap -> Libraries
toList libs =
   map (\(name, version) -> (name, version ^. CL.versionBranchL)) $ HM.toList libs
