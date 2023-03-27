{-# LANGUAGE FlexibleContexts #-}

module EffectZoo.Scenario.CountDown.PrEff.Program where

import Simple.State
import PrEff

program :: Member (State Int) eff => PrEff eff IVoid () () Int
program = do
  n <- get
  if n <= 0
    then pure n
    else do
      put (n - 1)
      program
