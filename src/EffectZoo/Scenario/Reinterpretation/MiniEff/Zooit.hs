{-# language DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
{-# LANGUAGE QualifiedDo #-}
module EffectZoo.Scenario.Reinterpretation.MiniEff.Zooit where

import MiniEff
import qualified Control.IxMonad as Ix
import EffectZoo.Scenario.Reinterpretation.MiniEff.Logging
import EffectZoo.Scenario.Reinterpretation.MiniEff.HTTP

data Zooit a where
  ListScenarios :: Zooit [String]

listScenarios :: Member Zooit effs => MiniEff effs s p p [String]
listScenarios = send ListScenarios

toLoggedHTTP :: MiniEff (Zooit ': effs) IVoid p q a -> MiniEff (HTTP ': Logging ': effs) IVoid p q a
toLoggedHTTP = reinterpret2 $ \ListScenarios -> Ix.do
  logMsg "Fetching a list of scenarios"
  lines <$> httpGET "/scenarios"
