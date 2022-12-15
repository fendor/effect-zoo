{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}

module EffectZoo.Scenario.FileSizes.MiniEff.File where

import           MiniEff
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared

data File a where
  TryFileSize :: FilePath -> File (Maybe Int)

tryFileSize :: Member File effs => FilePath -> MiniEff effs IVoid () () (Maybe Int)
tryFileSize = send . TryFileSize

fileIO :: Member IIO effs => MiniEff (File ': effs) IVoid () () a -> MiniEff effs IVoid () () a
fileIO = interpret (\(TryFileSize path) -> embedIO $ Shared.tryGetFileSize path)
