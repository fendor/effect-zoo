{-# OPTIONS_GHC -dsuppress-type-signatures #-}
module EffectZoo.Scenario.CountDown.MiniEff.Main where

import           EffectZoo.Scenario.CountDown.MiniEff.Program
import           Simple.State
import           MiniEff

countDown :: Int -> (Int, Int)
countDown initial = run (runState initial program)
