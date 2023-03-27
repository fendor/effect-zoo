{-# LANGUAGE FlexibleContexts #-}

module EffectZoo.Scenario.FileSizes.PrEff.Program where

import           PrEff
import           EffectZoo.Scenario.FileSizes.PrEff.File
import           EffectZoo.Scenario.FileSizes.PrEff.Logging

program :: (Member File effs, Member Logging effs) => [FilePath] -> PrEff effs IVoid () () Int
program files = do
  sizes <- mapM calculateFileSize files
  return (sum sizes)

calculateFileSize
  :: (Member File effs, Member Logging effs) => FilePath -> PrEff effs IVoid () () Int
calculateFileSize path = do
  logMsg ("Calculating the size of " ++ path)
  msize <- tryFileSize path
  case msize of
    Nothing   -> 0 <$ logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ logMsg (path ++ " is " ++ show size ++ " bytes")
