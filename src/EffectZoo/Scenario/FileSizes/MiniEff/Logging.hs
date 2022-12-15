{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}

module EffectZoo.Scenario.FileSizes.MiniEff.Logging where


import           Control.Monad.IO.Class
import           Data.IORef
import           MiniEff
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared

data Logging a where
  LogMsg :: String -> Logging ()

logMsg :: Member Logging effs => String -> MiniEff effs f p p ()
logMsg = send . LogMsg

logToIORef
  :: Member IIO effs
  => IORef [String]
  -> MiniEff (Logging ': effs) IVoid () () a
  -> MiniEff effs IVoid () () a
logToIORef r = interpret (\(LogMsg m) -> embedIO (Shared.logToIORef r m))
