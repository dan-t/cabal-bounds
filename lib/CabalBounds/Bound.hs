{-# Language PatternGuards #-}

module CabalBounds.Bound
   ( DropBound(..)
   , UpdateBound(..)
   , boundOfDrop
   , boundOfUpdate
   ) where

import CabalBounds.Args (Args(Drop, Update))
import qualified CabalBounds.Args as A
import CabalBounds.VersionComp (VersionComp(..), defaultLowerComp, defaultUpperComp)
import Data.Maybe (isJust)

data DropBound = DropUpper
               | DropBoth
               deriving (Show, Eq)


-- | The bound is only updated if it's missing.
type IfMissing = Bool

type LowerComp = VersionComp
type UpperComp = VersionComp

data UpdateBound = UpdateLower LowerComp IfMissing
                 | UpdateUpper UpperComp IfMissing
                 | UpdateBoth LowerComp UpperComp IfMissing
                 deriving (Show, Eq)


boundOfDrop :: Args -> DropBound
boundOfDrop Drop {A.upper = upper} = if upper then DropUpper else DropBoth
boundOfDrop _  = error "Expected Drop Args!"


boundOfUpdate :: Args -> UpdateBound
boundOfUpdate upd@Update {}
   | hasLower && hasUpper
   = UpdateBoth lowerComp upperComp ifMissing

   | hasLower
   = UpdateLower lowerComp ifMissing

   | hasUpper
   = UpdateUpper upperComp ifMissing

   | otherwise
   = UpdateBoth lowerComp upperComp ifMissing
   where
      lowerComp
         | Just comp <- A.lowerComp upd
         = comp

         | otherwise
         = defaultLowerComp

      upperComp
         | Just comp <- A.upperComp upd
         = comp

         | otherwise
         = defaultUpperComp

      ifMissing = A.missing upd

      hasLower = A.lower upd || (isJust . A.lowerComp $ upd)
      hasUpper = A.upper upd || (isJust . A.upperComp $ upd)

boundOfUpdate _ = error "Expected Update Args!"
