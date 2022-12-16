{-# LANGUAGE FlexibleContexts #-}

module EffectZoo.Scenario.CountDown.MiniEff.Program where

import Simple.State
import MiniEff

program :: Member (State Int) eff => MiniEff eff IVoid () () Int
program = do
  n <- get
  if n <= 0
    then pure n
    else do
      put (n - 1)
      program
