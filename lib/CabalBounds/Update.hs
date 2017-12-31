{-# Language CPP #-}

module CabalBounds.Update
   ( update
   ) where

import qualified Distribution.PackageDescription as D
import qualified Distribution.Package as P
import qualified Distribution.Version as V
import Control.Lens
import CabalBounds.Bound (UpdateBound(..))
import CabalBounds.Dependencies (Dependencies(..), filterDependency, dependencyIf)
import CabalBounds.VersionComp (VersionComp(..))
import CabalBounds.Types
import qualified CabalLenses as CL
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


update :: UpdateBound -> [CL.Section] -> Dependencies -> LibraryMap -> D.GenericPackageDescription -> D.GenericPackageDescription
update bound sections deps libs pkgDescrp =
   foldl' updateSection pkgDescrp sections
   where
      updateSection pkgDescrp section =
         pkgDescrp & dependencyIf condVars section . filterDep %~ updateDep

      filterDep = filterDependency deps
      updateDep = updateDependency bound libs
      condVars  = CL.fromDefaults pkgDescrp


updateDependency :: UpdateBound -> LibraryMap -> P.Dependency -> P.Dependency
updateDependency (UpdateLower comp ifMissing) libs dep =
   fromMaybe dep $
      if ifMissing && lowerBound_ /= CL.noLowerBound
         then Nothing
         else do
            lowerVersion <- HM.lookup pkgName_ libs
            let newLowerVersion = comp `compOf` lowerVersion
                newLowerBound   = V.LowerBound newLowerVersion V.InclusiveBound
                newIntervals    = (versionRange_ ^. CL.intervals) & _head . CL.lowerBound %~ updateIf (>) newLowerBound
                vrange          = mkVersionRange newIntervals
            return $ dep & CL.versionRange .~ vrange
   where
      pkgName_      = dep ^. CL.packageName . _Wrapped
      versionRange_ = dep ^. CL.versionRange
      lowerBound_   = fromMaybe CL.noLowerBound $ versionRange_ ^? CL.intervals . _head . CL.lowerBound

      updateIf f newBound oldBound
         | oldBound /= CL.noLowerBound
         = if f oldBound newBound
              then newBound
              else oldBound

         | otherwise
         = newBound


updateDependency (UpdateUpper comp ifMissing) libs dep =
   fromMaybe dep $
      if ifMissing && upperBound_ /= V.NoUpperBound
         then Nothing
         else do
            upperVersion <- HM.lookup pkgName_ libs
            let newUpperVersion = nextVersion comp upperVersion
                newUpperBound   = V.UpperBound newUpperVersion V.ExclusiveBound
                newIntervals    = (versionRange_ ^. CL.intervals) & _last . CL.upperBound %~ updateIf (<) newUpperBound
                vrange          = mkVersionRange newIntervals
            return $ dep & CL.versionRange .~ vrange
   where
      versionRange_ = dep ^. CL.versionRange
      pkgName_      = dep ^. CL.packageName . _Wrapped
      upperBound_   = fromMaybe V.NoUpperBound $ versionRange_ ^? CL.intervals . _last . CL.upperBound

      updateIf f newBound oldBound
         | oldBound /= V.NoUpperBound
         = if f oldBound newBound
              then newBound
              else oldBound

         | otherwise
         = newBound


updateDependency (UpdateBoth lowerComp upperComp ifMissing) libs dep =
    updateDependency (UpdateLower lowerComp ifMissing) libs $
    updateDependency (UpdateUpper upperComp ifMissing) libs dep


compOf :: VersionComp -> V.Version -> V.Version
Major1 `compOf` version =
   version & CL.versionBranchL %~ take 1

Major2 `compOf` version =
   version & CL.versionBranchL %~ take 2

Minor `compOf` version =
   version


nextVersion :: VersionComp -> V.Version -> V.Version
nextVersion comp version =
   (comp `compOf` version)
      & CL.versionBranchL         %~ ensureMinimalVersionBranch comp
      & CL.versionBranchL . _last %~ (+ 1)


ensureMinimalVersionBranch :: VersionComp -> [Int] -> [Int]
ensureMinimalVersionBranch comp branch =
   let numDigits  = numNeededVersionDigits comp
       numMissing = numDigits - length branch
       branch' | numMissing > 0 = branch ++ replicate numMissing 0
               | otherwise      = branch
       in branch'
   where
      numNeededVersionDigits Major1 = 1
      numNeededVersionDigits Major2 = 2
      numNeededVersionDigits Minor  = 3


mkVersionRange :: [V.VersionInterval] -> V.VersionRange
mkVersionRange []  = V.anyVersion
mkVersionRange vis = V.fromVersionIntervals . V.mkVersionIntervals $ vis
