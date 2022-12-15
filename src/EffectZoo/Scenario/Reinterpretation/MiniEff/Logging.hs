{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module EffectZoo.Scenario.Reinterpretation.MiniEff.Logging where

import MiniEff
import Simple.Writer

data Logging a where
  LogMsg :: String -> Logging ()

logMsg :: Member Logging effs => String -> MiniEff effs s p p ()
logMsg = send . LogMsg

accumulateLogMessages :: MiniEff (Logging ': effs) IVoid () () a -> MiniEff (Writer [String] ': effs) IVoid () () a
accumulateLogMessages = reinterpret $ \(LogMsg m) -> tell [m]
