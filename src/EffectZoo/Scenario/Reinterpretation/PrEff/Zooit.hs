{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
{-# LANGUAGE QualifiedDo #-}
module EffectZoo.Scenario.Reinterpretation.PrEff.Zooit where

import PrEff
import qualified Control.IxMonad as Ix
import EffectZoo.Scenario.Reinterpretation.PrEff.Logging
import EffectZoo.Scenario.Reinterpretation.PrEff.HTTP

data Zooit a where
  ListScenarios :: Zooit [String]

listScenarios :: Member Zooit effs => PrEff effs s p p [String]
listScenarios = send ListScenarios

toLoggedHTTP :: PrEff (Zooit ': effs) IVoid p q a -> PrEff (HTTP ': Logging ': effs) IVoid p q a
toLoggedHTTP = reinterpret2 $ \ListScenarios -> Ix.do
  logMsg "Fetching a list of scenarios"
  lines <$> httpGET "/scenarios"
