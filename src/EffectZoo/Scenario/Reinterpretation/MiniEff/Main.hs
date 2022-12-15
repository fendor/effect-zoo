module EffectZoo.Scenario.Reinterpretation.MiniEff.Main where

import Control.IxMonad as Ix
import MiniEff
import Simple.Reader
import Simple.Writer
import Data.Function
import EffectZoo.Scenario.Reinterpretation.MiniEff.HTTP
import EffectZoo.Scenario.Reinterpretation.MiniEff.Logging
import EffectZoo.Scenario.Reinterpretation.MiniEff.Zooit
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
