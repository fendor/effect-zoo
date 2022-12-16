{-# LANGUAGE FlexibleContexts #-}
module EffectZoo.Scenario.BigStack.MiniEff.Program where

import Simple.Reader
import Simple.State
import MiniEff
import qualified Control.IxMonad as Ix
import           Control.Monad

program
  :: (Member (State Int) effs, Member (Reader Int) effs)
  => MiniEff effs IVoid () () ()
program = do
  n <- ask
  replicateM_ n (modify (+ n))
