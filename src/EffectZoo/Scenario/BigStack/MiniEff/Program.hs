{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
module EffectZoo.Scenario.BigStack.MiniEff.Program where

import Simple.Reader
import Simple.State
import MiniEff
import qualified Control.IxMonad as Ix

program
  :: (Member (State Int) effs, Member (Reader Int) effs)
  => MiniEff effs IVoid () () ()
program = Ix.do
  n <- ask
  Ix.replicateM_ n (modify (+ n))
