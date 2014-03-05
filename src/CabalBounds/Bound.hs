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

data UpdateBound = UpdateLower VersionComp
                 | UpdateUpper VersionComp
                 | UpdateBoth { lowerComp :: VersionComp, upperComp :: VersionComp }
                 deriving (Show, Eq)


boundOfDrop :: Args -> DropBound
boundOfDrop Drop {A.upper = upper} = if upper then DropUpper else DropBoth
boundOfDrop _  = error "Expected Drop Args!"


boundOfUpdate :: Args -> UpdateBound
boundOfUpdate upd@Update {}
   | hasLower && hasUpper
   = UpdateBoth lowerComp upperComp

   | hasLower
   = UpdateLower lowerComp

   | hasUpper
   = UpdateUpper upperComp

   | otherwise
   = UpdateBoth lowerComp upperComp
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

      hasLower = A.lower upd || (isJust . A.lowerComp $ upd)
      hasUpper = A.upper upd || (isJust . A.upperComp $ upd)

boundOfUpdate _ = error "Expected Update Args!"
