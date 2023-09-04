{-# OPTIONS_GHC -dsuppress-type-signatures #-}
module EffectZoo.Scenario.CountDown.PrEff.Main where

import           EffectZoo.Scenario.CountDown.PrEff.Program
import           PrEff.Simple.State
import           PrEff

countDown :: Int -> (Int, Int)
countDown initial = run (runState initial program)
