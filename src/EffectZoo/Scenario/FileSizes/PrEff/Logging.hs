{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}

module EffectZoo.Scenario.FileSizes.PrEff.Logging where


import           Control.Monad.IO.Class
import           Data.IORef
import           PrEff
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared

data Logging a where
  LogMsg :: String -> Logging ()

logMsg :: Member Logging effs => String -> PrEff effs f p p ()
logMsg = send . LogMsg

logToIORef
  :: Member IIO effs
  => IORef [String]
  -> PrEff (Logging ': effs) IVoid () () a
  -> PrEff effs IVoid () () a
logToIORef r = interpret (\(LogMsg m) -> embedIO (Shared.logToIORef r m))
