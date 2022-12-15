{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}

module EffectZoo.Scenario.CountDown.MiniEff.Program where

import Simple.State
import MiniEff
import Control.IxMonad as Ix

program :: Member (State Int) eff => MiniEff eff IVoid () () Int
program = Ix.do
  n <- get
  if n <= 0
    then Ix.pure n
    else Ix.do
      put (n - 1)
      program
