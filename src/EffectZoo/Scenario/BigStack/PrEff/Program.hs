{-# LANGUAGE FlexibleContexts #-}
module EffectZoo.Scenario.BigStack.PrEff.Program where

import Simple.Reader
import Simple.State
import PrEff
import qualified Control.IxMonad as Ix
import           Control.Monad

program
  :: (Member (State Int) effs, Member (Reader Int) effs)
  => PrEff effs IVoid () () ()
program = do
  n <- ask
  replicateM_ n (modify (+ n))
