{-# LANGUAGE DeriveDataTypeable #-}

module CabalBounds.VersionComp
   ( VersionComp(..)
   , defaultLowerComp
   , defaultUpperComp
   ) where

import Data.Data (Data(..), Typeable(..))

-- | The component of a version number A.B.C -> Major1.Major2.Minor
data VersionComp = Major1
                 | Major2
                 | Minor
                 deriving (Data, Typeable, Eq, Show)


defaultLowerComp :: VersionComp
defaultLowerComp = Minor


defaultUpperComp :: VersionComp
defaultUpperComp = Major2
