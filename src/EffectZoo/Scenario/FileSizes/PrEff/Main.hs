module EffectZoo.Scenario.FileSizes.PrEff.Main where

import           PrEff
import           Data.IORef
import           EffectZoo.Scenario.FileSizes.PrEff.File
import           EffectZoo.Scenario.FileSizes.PrEff.Logging
import           EffectZoo.Scenario.FileSizes.PrEff.Program

calculateFileSizes :: [FilePath] -> IO (Int, [String])
calculateFileSizes files = do
  logs      <- newIORef []
  size      <- runIO (fileIO (logToIORef logs (program files)))
  finalLogs <- readIORef logs
  return (size, finalLogs)
