{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module EffectZoo.Scenario.FileSizes.MiniEff.Program where

import           MiniEff
import qualified Control.IxMonad as Ix
import           EffectZoo.Scenario.FileSizes.MiniEff.File
import           EffectZoo.Scenario.FileSizes.MiniEff.Logging

program :: (Member File effs, Member Logging effs) => [FilePath] -> MiniEff effs IVoid () () Int
program files = Ix.do
  sizes <- Ix.mapM calculateFileSize files
  Ix.return (sum sizes)

calculateFileSize
  :: (Member File effs, Member Logging effs) => FilePath -> MiniEff effs IVoid () () Int
calculateFileSize path = Ix.do
  logMsg ("Calculating the size of " ++ path)
  msize <- tryFileSize path
  case msize of
    Nothing   -> 0 <$ logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ logMsg (path ++ " is " ++ show size ++ " bytes")
