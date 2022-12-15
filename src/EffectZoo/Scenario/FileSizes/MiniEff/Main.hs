module EffectZoo.Scenario.FileSizes.MiniEff.Main where

import           MiniEff
import           Data.IORef
import           EffectZoo.Scenario.FileSizes.MiniEff.File
import           EffectZoo.Scenario.FileSizes.MiniEff.Logging
import           EffectZoo.Scenario.FileSizes.MiniEff.Program

calculateFileSizes :: [FilePath] -> IO (Int, [String])
calculateFileSizes files = do
  logs      <- newIORef []
  size      <- runIO (fileIO (logToIORef logs (program files)))
  finalLogs <- readIORef logs
  return (size, finalLogs)
