{-# LANGUAGE FlexibleContexts #-}

module EffectZoo.Scenario.FileSizes.MiniEff.Program where

import           MiniEff
import           EffectZoo.Scenario.FileSizes.MiniEff.File
import           EffectZoo.Scenario.FileSizes.MiniEff.Logging

program :: (Member File effs, Member Logging effs) => [FilePath] -> MiniEff effs IVoid () () Int
program files = do
  sizes <- mapM calculateFileSize files
  return (sum sizes)

calculateFileSize
  :: (Member File effs, Member Logging effs) => FilePath -> MiniEff effs IVoid () () Int
calculateFileSize path = do
  logMsg ("Calculating the size of " ++ path)
  msize <- tryFileSize path
  case msize of
    Nothing   -> 0 <$ logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ logMsg (path ++ " is " ++ show size ++ " bytes")
