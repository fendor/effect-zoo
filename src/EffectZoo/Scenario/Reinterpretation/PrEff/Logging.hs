{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module EffectZoo.Scenario.Reinterpretation.PrEff.Logging where

import PrEff
import PrEff.Simple.Writer

data Logging a where
  LogMsg :: String -> Logging ()

logMsg :: Member Logging effs => String -> PrEff effs s p p ()
logMsg = send . LogMsg

accumulateLogMessages :: PrEff (Logging ': effs) IVoid () () a -> PrEff (Writer [String] ': effs) IVoid () () a
accumulateLogMessages = reinterpret $ \(LogMsg m) -> tell [m]
