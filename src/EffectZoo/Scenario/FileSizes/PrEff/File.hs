{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}

module EffectZoo.Scenario.FileSizes.PrEff.File where

import           PrEff
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared

data File a where
  TryFileSize :: FilePath -> File (Maybe Int)

tryFileSize :: Member File effs => FilePath -> PrEff effs IVoid () () (Maybe Int)
tryFileSize = send . TryFileSize

fileIO :: Member IIO effs => PrEff (File ': effs) IVoid () () a -> PrEff effs IVoid () () a
fileIO = interpret (\(TryFileSize path) -> embedIO $ Shared.tryGetFileSize path)
