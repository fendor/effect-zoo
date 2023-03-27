module EffectZoo.Scenario.Reinterpretation.PrEff.Main where

import Control.IxMonad as Ix
import PrEff
import Simple.Reader
import Simple.Writer
import Data.Function
import EffectZoo.Scenario.Reinterpretation.PrEff.HTTP
import EffectZoo.Scenario.Reinterpretation.PrEff.Logging
import EffectZoo.Scenario.Reinterpretation.PrEff.Zooit
                                               as Zooit
import EffectZoo.Scenario.Reinterpretation.Shared

listScenarios :: Int -> ([String], [String])
listScenarios n =
  fmap concat (Ix.replicateM n Zooit.listScenarios)
    & toLoggedHTTP
    & runReader response
    . mockResponses
    & runWriter
    . accumulateLogMessages
    & run
